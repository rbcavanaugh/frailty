#===============================================================================
# ################################ PolyPharmacy ################################
# ==============================================================================



calculate_pp <- function() {

    if(data_source == "allofus"){

        concept = tbl(con, "concept")
        drug_exp = tbl(con, "drug_exposure")
        concept_an = tbl(con, "concept_ancestor")


    } else {

        # PMTX
        concept = tbl(con, inDatabaseSchema(cdm_schema, "concept"))
        drug_exp = tbl(con, inDatabaseSchema(cdm_schema, "drug_exposure"))
        concept_an = tbl(con, inDatabaseSchema(cdm_schema, "concept_ancestor"))

    }

    # date functions switch between redshift and bigrquery.
    pp_lookback <- switch (dbms,
                           "redshift" = glue::glue("DATEADD(YEAR, -1, person_end_date)"),
                           "bigquery" = glue::glue("DATE_ADD(person_end_date, INTERVAL -1 YEAR)"),
                           rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )

    pp_datediff <- switch (dbms,
                           "redshift" = glue::glue("DATEDIFF(DAY,  drug_exposure_start_date, drug_exposure_end_date)"),
                           "bigquery" = glue::glue("DATE_DIFF(drug_exposure_end_date, person_start_date, DAY)"),
                           rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )

    # date functions for polypharmacy adjustment for drug_exposure table below.
    add_days_supply <- switch (dbms,
                               "redshift" = glue::glue("DATEADD(DAY, days_supply, drug_exposure_start_date)"),
                               "bigquery" = glue::glue("DATE_ADD(drug_exposure_start_date, INTERVAL days_supply DAY)"),
                               rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )
    add_1_day <- switch (dbms,
                         "redshift" = glue::glue("DATEADD(DAY, 1, drug_exposure_start_date)"),
                         "bigquery" = glue::glue("DATE_ADD(drug_exposure_start_date, INTERVAL 1 DAY)"),
                         rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )


    #antibiotics
    antibiotics = concept %>%
        filter(vocabulary_id == "RxNorm", concept_class_id == "Ingredient") %>%
        inner_join(
            concept_an %>%
                filter(ancestor_concept_id == 21602796),
            by = join_by(concept_id == descendant_concept_id)
        ) %>%
        filter(as.Date("2024-07-15") >= valid_start_date,
               as.Date("2024-07-15") <= valid_end_date) %>%
        select(ingredient_concept_id = concept_id, ingredient_concept_name = concept_name, ingredient_concept_code = concept_code)

    # initial drugs
    drugs = concept %>%
        filter(concept_class_id == "Ingredient", vocabulary_id == "RxNorm") %>%
        inner_join(concept_an ,
                   by = join_by(concept_id == ancestor_concept_id))

    # rebuild drug era table (essentially)
    drug_era2 = drug_exp %>%
        filter(drug_concept_id != 0, days_supply >= 0 | is.na(days_supply)) %>%
        inner_join(drugs, by = join_by(drug_concept_id == descendant_concept_id)) %>%
        select(drug_exposure_id, person_id, ingredient_concept_id = concept_id, drug_exposure_start_date, drug_exposure_end_date, days_supply) %>%
        anti_join(antibiotics, by = join_by(ingredient_concept_id)) %>%
        # this replaces the coalesce function from the SQL code that I couldn't get to work.
        mutate(
            drug_exposure_end_date = case_when(
                !is.na(drug_exposure_end_date) ~ drug_exposure_end_date,
                (!is.na(days_supply) & days_supply > 0) ~ dplyr::sql(!!add_days_supply),
                TRUE ~ dplyr::sql(!!add_1_day)
            )
        )

    # put together with the cohort and get all people wiht more than 10 ingredients in teh time span.
    pp = drug_era2 %>%
        inner_join(cohort_all, by = "person_id", x_as = "pp1", y_as = "pp2") %>%
        select(person_start_date = visit_lookback_date, person_end_date = index_date,
               person_id, ingredient_concept_id , drug_exposure_start_date , drug_exposure_end_date ) |>
        mutate(drug_search_start_date = dplyr::sql(!!pp_lookback),
               era_date_diff = dplyr::sql(!!pp_datediff))  |>
        filter(
            (drug_exposure_start_date >= drug_search_start_date & drug_exposure_start_date <= person_end_date) | (drug_exposure_end_date >= drug_search_start_date & drug_exposure_end_date <= person_end_date),
            era_date_diff >= 1
        ) |>
        distinct(person_id, ingredient_concept_id) |>
        count(person_id) |>
        filter(n >= 10) |>
        mutate(score = 1,
               category = "Polypharmacy",
        ) |>
        inner_join(cohort_all, by = "person_id", x_as = "pp3", y_as = "pp4") |>
        select(
            person_id,
            is_female,
            age_group,
            score,
            category
        )


    return(pp)

}


if(data_source == "allofus"){

    pp = calculate_pp() %>% collect()
    pp_tmp = allofus::aou_create_temp_table(pp, nchar_batch = 1e5)

} else {

    if(DatabaseConnector::existsTable(con, my_schema, "frailty_cohort_polypharmacy")){
        cat("polypharmacy table already exists")
    } else {
        pp = calculate_pp()
        CDMConnector::computeQuery(pp, "frailty_cohort_polypharmacy", temporary = FALSE, schema = my_schema, overwrite = TRUE)
    }

}


# # calculate prevalence.
# pp_prevalence = cohort_all %>%
#     left_join(pp %>% select(person_id, score), by = "person_id") %>%
#     mutate(score = ifelse(is.na(score), 0, 1)) %>%
#     count(score, is_female, age_group) %>%
#     collect() %>%
#     group_by(is_female, age_group) %>%
#     mutate(total = sum(n)) %>%
#     ungroup() %>%
#     filter(score == 1) %>%
#     mutate(pct = scales::label_percent()(n/total))
#
# # numbers for Chen
# # 0
# pp2 = drug_era2 %>%
#     inner_join(cohort_all, by = "person_id", x_as = "pp1", y_as = "pp2") %>%
#     select(person_start_date = visit_lookback_date, person_end_date = index_date,
#            person_id, ingredient_concept_id , drug_exposure_start_date , drug_exposure_end_date ) |>
#     mutate(drug_search_start_date = dplyr::sql(!!pp_lookback),
#            era_date_diff = dplyr::sql(!!pp_datediff))  |>
#     filter(
#         (drug_exposure_start_date >= drug_search_start_date & drug_exposure_start_date <= person_end_date) | (drug_exposure_end_date >= drug_search_start_date & drug_exposure_end_date <= person_end_date),
#         era_date_diff >= 1
#     ) |>
#     distinct(person_id, ingredient_concept_id) |>
#     count(person_id)
#
# cohort_all %>%
#     select(person_id) %>%
#     left_join(pp2, by = "person_id") %>%
#     mutate(n = ifelse(is.na(n), 0, n)) -> pp_0
#
# pp_0c = collect(pp_0)
# quantile(pp_0c$n, probs = seq(0, 1, 0.1))




