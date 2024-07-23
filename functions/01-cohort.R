
# ============================================================================
# ################################ COHORT DEFINITION #########################
# ============================================================================

if(DatabaseConnector::existsTable(con, my_schema, "frailty_cohort_clean")){
    cat("cohort table already exists")
} else {

        library(tidyverse)
        library(ohdsilab)
        library(DBI)
        library(DatabaseConnector)
        library(CDMConnector)
        library(glue)

        # cdm_schema is the omop db
        # my_schema is the user write schema

        # Cohort Generation

        # Note - this only needs to be run once, if the intermediate steps are saved to
        # persistent tables in the user schema

        # Join person table to visit occurrence table
        # Pick a random visit. I think this SqlRender::translate() should make this a bit
        # more flexible to the different dbms...
        index_date_query <- tbl(con, inDatabaseSchema(cdm_schema, "person")) |>
            select(person_id, year_of_birth, gender_concept_id) |>
            omop_join("visit_occurrence", type = "inner", by = "person_id") |>
            select(person_id, index_date = visit_start_date, year_of_birth, gender_concept_id) |>
            mutate(rand_index = sql(SqlRender::translate("RAND()", con@dbms)[[1]])) |>
            slice_min(n = 1, by = person_id, order_by = rand_index) |>
            select(-rand_index)

        # From that query, make sure there are 365 days preceeding to the observation_period
        # start date. Filter for age at index date is >= 40
        #!!!!!!!!!!! Changed to: >= 365 below !!!!!!!!!!!!#
        cohort <- index_date_query |>
            omop_join("observation_period", type = "inner", by = "person_id") |>
            filter(dateDiff("day", observation_period_start_date, index_date) >= 365) |>
            select(person_id, year_of_birth, gender_concept_id, index_date, observation_period_start_date, observation_period_end_date) |>
            mutate(age = year(index_date) - year_of_birth,
                   yob_imputed = ifelse(year_of_birth < 1938, 1, 0)) |>
            filter(age >= 40)

        # test = cohort |> dbi_collect()

        # saving as a persistent table in my schema as a midpoint/ intermediate table. This could be a
        # temporary table if needed.
        ohdsilab::set_seed(0.5)
        #CDMConnector::computeQuery(cohort, "frailty_cohort", temporary = temporary_intermediate_steps, schema = my_schema, overwrite = TRUE)



        # ============================================================================
        # ################################ COHORT #######################################
        # ============================================================================
        # Do it with everyone just omop VAFI
        cohort_ids <- tbl(con, inDatabaseSchema(my_schema, "frailty_cohort")) |>
            distinct(person_id)

        cohort_all <- tbl(con, inDatabaseSchema(my_schema, "frailty_cohort")) %>%
            mutate(age = ifelse(yob_imputed == 1, 84, age)) %>%
            mutate(
                is_female = ifelse(gender_concept_id == 8507, 0, 1),
                age_group = cut(age,
                                breaks = c(40,  45,  50,  55, 60,  65,  70,  75,  80,  100),
                                right = FALSE,
                                include.lowest = TRUE),
                visit_lookback_date = !!CDMConnector::dateadd("index_date", -1, interval = "year")
            ) |>
            select(person_id, is_female, age_group, visit_lookback_date, index_date, yob_imputed)|>
            inner_join(cohort_ids, by = "person_id") |>
            group_by(person_id) |>
            filter(index_date == min(index_date)) |>
            ungroup()

        CDMConnector::computeQuery(pp, "frailty_cohort_polypharmacy", temporary = FALSE, schema = my_schema, overwrite = TRUE)

}


