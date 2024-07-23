
library(tidyverse)
library(ohdsilab)
library(DBI)
library(DatabaseConnector)
library(CDMConnector)
library(glue)

# make sure you install the latest aouFI repo
# remotes::install_github("roux-ohdsi/aouFI")

# set to data source and dbms system.
# This script works for everything except All of Us. (Probably).
data_source = "pharmetrics"
dbms = "redshift"

# ============================================================================
# ################################ Setup #######################################
# ============================================================================

    # connection
    source(here::here("functions", "connection_setup.R"))

    # get cohort'
    source(here::here("functions", "01-cohort.R"))
    cohort_all = tbl(con, inDatabaseSchema(my_schema, "frailty_cohort_clean")) # PMTX
    # get pp data
    source(here::here("functions", "02-polypharmacy-aou.R"))

    # setup FI in pharmetrics

    if(!DatabaseConnector::existsTable(con, my_schema, "vafi_rev2")){

        # creating fi tables
        vafi_lb = aouFI::vafi_rev %>% select(category, concept_id, score) %>%
            left_join(aouFI::lb %>% filter(fi == "vafi"), by = "category")

        insertTable_chunk(vafi_lb, "vafi_rev2")
    }


    if(DatabaseConnector::existsTable(con, my_schema, "efi_rev2")){
        # creating fi tables
        efi_lb = aouFI::fi_indices %>% filter(fi == "efi_sno") %>% select(-fi) %>%
            left_join(aouFI::lb %>% filter(fi == "efi"), by = "category")

        insertTable_chunk(efi_lb, "efi_rev2")
    }


    source(here::here("functions", "summary_functions.R"))



# ============================================================================
# ################################ VAFI #######################################
# ============================================================================


vafi_all <- omop2fi_lb(con = con,
                       schema = cdm_schema,
                       index = "vafi",
                       .data_search = cohort_all,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       dbms = dbms,
                       concept_location = tbl(con, inDatabaseSchema(my_schema, "vafi_rev2")) |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 1
) |>
    distinct(person_id, age_group, is_female, score, category)

dplyr::compute(vafi_all, inDatabaseSchema(my_schema, "vafi_all_aa"),
               temporary = FALSE,
               overwrite = TRUE)

rm(vafi_all)

# ============================================================================
# ################################ VAFI VARIABLE LOOKBACK ####################
# ============================================================================


vafi_all <- omop2fi_lb(con = con,
                       schema = cdm_schema,
                       index = "vafi",
                       .data_search = cohort_all,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       dbms = dbms,
                       concept_location = tbl(con, inDatabaseSchema(my_schema, "vafi_rev2")) |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 3
) |>
    distinct(person_id, age_group, is_female, score, category)


dplyr::compute(vafi_all, inDatabaseSchema(my_schema, "vafi_all_ac"),
               temporary = FALSE,
               overwrite = TRUE)

rm(vafi_all)


# ============================================================================
# ################################ VAFI SUMMARIZING #######################################
# ============================================================================
cohort_c = cohort_all |> select(person_id, age_group, is_female) %>% collect()


# 1 year acute acute
vafi_all = tbl(con, inDatabaseSchema(my_schema, "vafi_all_aa"))

# add robust individuals back
vafi_all_summary <- fi_with_robust(
    fi_query = vafi_all,
    cohort = cohort_all,
    denominator = 31, lb = 0.11, ub = 0.21)

# summarize
t = summarize_fi(vafi_all_summary) %>% collect()
write.csv(t, glue("KI/{Sys.Date()}_vafi_acute1-chronic1_{data_source}.csv"), row.names = FALSE)

vafi_cats = aouFI::vafi_rev %>% distinct(category) %>% pull(category)
vafi_c = vafi_all %>% select(person_id, category) %>% collect() %>% mutate(score = 1)

vafi_cat_summary = summarize_cats(
    vafi_c,
    cohort = cohort_c,
    cats = vafi_cats
    ) %>%
    arrange(category, age_group, is_female) %>%
    drop_na() %>%
    mutate(count = ifelse(count < 20, 0, count),
           percent = ifelse(count < 20, 0, percent))

write.csv(vafi_cat_summary, glue("KI/{Sys.Date()}_vafi_categories_acute1-chronic1_{data_source}.csv"), row.names = FALSE)

rm(t)
rm(vafi_cat_summary)
rm(vafi_all)
rm(vafi_c)
gc()

# 1-3 year acute chronic
vafi_all = tbl(con, inDatabaseSchema(my_schema, "vafi_all_ac"))

# add robust individuals back
vafi_all_summary <- fi_with_robust(
    fi_query = vafi_all,
    cohort = cohort_all,
    denominator = 31, lb = 0.11, ub = 0.21)

# summarize
t = summarize_fi(vafi_all_summary) %>% collect()
write.csv(t, glue("KI/{Sys.Date()}_vafi_acute1-chronic3_{data_source}.csv"), row.names = FALSE)

vafi_cats = aouFI::vafi_rev %>% distinct(category) %>% pull(category)
vafi_c = vafi_all %>% select(person_id, category) %>% collect() %>% mutate(score = 1)

vafi_cat_summary = summarize_cats(
    vafi_c,
    cohort = cohort_c,
    cats = vafi_cats
    ) %>%
    arrange(category, age_group, is_female) %>%
    drop_na() %>%
    mutate(count = ifelse(count < 20, 0, count),
           percent = ifelse(count < 20, 0, percent))

write.csv(vafi_cat_summary, glue("KI/{Sys.Date()}_vafi_categories_acute1-chronic3_{data_source}.csv"), row.names = FALSE)


rm(t)
rm(vafi_cat_summary)
rm(vafi_all)
rm(vafi_c)
gc()
# may want to clear memory at this point...

# ============================================================================
# ################################ EFI #######################################
# ============================================================================

efi_all <- aouFI::omop2fi_lb(con = con,
                             schema = cdm_schema,
                             index = "efi",
                             .data_search = cohort_all,
                             search_person_id = "person_id",
                             search_start_date = "visit_lookback_date",
                             search_end_date = "index_date",
                             keep_columns = c("age_group", "is_female"),
                             collect = FALSE,
                             dbms = dbms,
                             unique_categories = TRUE,
                             concept_location = tbl(con, inDatabaseSchema(my_schema, "efi_rev2")) |> rename(chronic_category = lookback),
                             acute_lookback = 1,
                             chronic_lookback = 1
) |>
    distinct(person_id, age_group, is_female, score, category)

union_all(
    efi_all,
    tbl(con, inDatabaseSchema(my_schema, "frailty_cohort_polypharmacy"))
) %>% distinct() -> efi_all

dplyr::compute(efi_all, inDatabaseSchema(my_schema, "efi_all_aa"),
               temporary = FALSE,
               overwrite = TRUE)


rm(efi_all)

# ============================================================================
# ################################ EFI AC #######################################
# ============================================================================

efi_all <- omop2fi_lb(con = con,
                       schema = cdm_schema,
                       index = "efi",
                       .data_search = cohort_all,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       dbms = dbms,
                       concept_location = tbl(con, inDatabaseSchema(my_schema, "efi_rev2")) |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 3
) |>
    distinct(person_id, age_group, is_female, score, category)

union_all(
    efi_all,
    tbl(con, inDatabaseSchema(my_schema, "frailty_cohort_polypharmacy"))
) %>% distinct() -> efi_all

# save result of query as intermediate step #2

dplyr::compute(efi_all, inDatabaseSchema(my_schema, "efi_all_ac"),
               temporary = FALSE,
               overwrite = TRUE)

rm(efi_all)

# ============================================================================
# ################################ EFI SUMMARIZING #######################################
# ============================================================================

# 1 year lookback
efi_all = tbl(con, inDatabaseSchema(my_schema, "efi_all_aa"))

# add robust individuals back
efi_all_summary <- fi_with_robust(
    fi_query = efi_all,
    cohort = cohort_all,
    denominator = 35, lb = 0.12, ub = 0.24)

# summarize
t = summarize_fi(efi_all_summary) %>% collect()
write.csv(t, glue("KI/{Sys.Date()}_efi_acute1-chronic1_{data_source}.csv"), row.names = FALSE)

efi_cats = aouFI::fi_indices %>% filter(fi == "efi_sno") %>% distinct(category) %>% pull(category)
efi_c = efi_all %>% select(person_id, category, score) %>% collect()
# cohort_c from above with vafi

efi_cat_summary = summarize_cats(
    efi_c,
    cohort = cohort_c,
    cats = efi_cats
    ) %>%
    arrange(category, age_group, is_female) %>%
    drop_na() %>%
    mutate(count = ifelse(count < 20, 0, count),
           percent = ifelse(count < 20, 0, percent))

write.csv(efi_cat_summary, glue("KI/{Sys.Date()}_efi_categories_acute1-chronic1_{data_source}.csv"), row.names = FALSE)


rm(t)
rm(efi_cat_summary)
rm(efi_c)
gc()



# Variable year lookback
efi_all = tbl(con, inDatabaseSchema(my_schema, "efi_all_ac"))

# add robust individuals back
efi_all_summary <- fi_with_robust(
    fi_query = efi_all,
    cohort = cohort_all,
    denominator = 35, lb = 0.12, ub = 0.24)

# summarize
t = summarize_fi(efi_all_summary) %>% collect()
write.csv(t, glue("KI/{Sys.Date()}_efi_acute1-chronic3_{data_source}.csv"), row.names = FALSE)

efi_cats = aouFI::fi_indices %>% filter(fi == "efi_sno") %>% distinct(category) %>% pull(category)
efi_c = efi_all %>% select(person_id, category, score) %>% collect()
# cohort_c from above with vafi

efi_cat_summary = summarize_cats(
    efi_c,
    cohort = cohort_c,
    cats = efi_cats
) %>%
    arrange(category, age_group, is_female) %>%
    drop_na() %>%
    mutate(count = ifelse(count < 20, 0, count),
           percent = ifelse(count < 20, 0, percent))

write.csv(efi_cat_summary, glue("KI/{Sys.Date()}_efi_categories_acute1-chronic3_{data_source}.csv"), row.names = FALSE)



