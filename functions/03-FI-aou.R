
library(tidyverse)
library(DBI)
library(DatabaseConnector)
library(CDMConnector)
library(glue)
library(allofus)
library(aouFI)

# set to pharmetrics or allofus.
data_source = "allofus"
dbms = "bigquery" # bigrquery DBI connection doesn't hold the information the same way.

# ============================================================================
# ################################ SETUP #######################################
# ============================================================================

    library(allofus)
    con <- allofus::aou_connect(quiet = TRUE)

    # get cohort
    source(here::here("functions", "01-cohort-aou.R"))
    cohort_all = demo
    # get pp
    # takes a hot second
    source(here::here("functions", "02-polypharmacy-aou.R"))

    # creating fi tables
    vafi_rev2 = aouFI::vafi_rev %>% select(category, concept_id, score) %>%
        left_join(aouFI::lb %>% filter(fi == "vafi"), by = "category") %>%
        aou_create_temp_table(nchar_batch = 1e5)

    efi_rev2 = aouFI::fi_indices %>% filter(fi == "efi_sno") %>% select(-fi) %>%
        left_join(aouFI::lb %>% filter(fi == "efi"), by = "category") %>%
        aou_create_temp_table(nchar_batch = 1e5)

# ============================================================================
# ################################ VAFI #######################################
# ============================================================================


vafi_all_aa <- omop2fi_lb(con = con,
                       #schema = cdm_schema,
                       index = "vafi",
                       .data_search = cohort_all,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       dbms = "bigquery",
                       concept_location = vafi_rev2 |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 1
) |>
    distinct(person_id, age_group, is_female, score, category)

# ============================================================================
# ################################ VAFI VARIABLE LOOKBACK ####################
# ============================================================================


vafi_all_ac <- omop2fi_lb(con = con,
                          #schema = cdm_schema,
                          index = "vafi",
                          .data_search = cohort_all,
                          search_person_id = "person_id",
                          search_start_date = "visit_lookback_date",
                          search_end_date = "index_date",
                          keep_columns = c("age_group", "is_female"),
                          collect = FALSE,
                          unique_categories = TRUE,
                          dbms = "bigquery",
                          concept_location = vafi_rev2 |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 3
) |>
    distinct(person_id, age_group, is_female, score, category)

# ============================================================================
# ################################ VAFI SUMMARIZING #######################################
# ============================================================================
cohort_c = cohort_all |> select(person_id, age_group, is_female) %>% collect()

# 1 year acute acute
vafi_all = vafi_all_aa

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
vafi_all = vafi_all_ac

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
                             index = "efi",
                             .data_search = cohort_all,
                             search_person_id = "person_id",
                             search_start_date = "visit_lookback_date",
                             search_end_date = "index_date",
                             keep_columns = c("age_group", "is_female"),
                             collect = FALSE,
                             unique_categories = TRUE,
                             dbms = "bigquery",
                             concept_location = efi_rev2 |> rename(chronic_category = lookback),
                             acute_lookback = 1,
                             chronic_lookback = 1
) |>
    distinct(person_id, age_group, is_female, score, category)

union_all(
    efi_all,
    pp_tmp
) %>% distinct() -> efi_all_aa

rm(efi_all)

# ============================================================================
# ################################ EFI AC #######################################
# ============================================================================

efi_all <- omop2fi_lb(con = con,
                       index = "efi",
                       .data_search = cohort_all,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       dbms = "bigquery",
                       concept_location = efi_rev2 |> rename(chronic_category = lookback),
                       acute_lookback = 1,
                       chronic_lookback = 3
) |>
    distinct(person_id, age_group, is_female, score, category)

union_all(
    efi_all,
    pp_tmp
) %>% distinct() -> efi_all_ac

rm(efi_all)

# ============================================================================
# ################################ EFI SUMMARIZING #######################################
# ============================================================================

# 1 year lookback
efi_all = efi_all_aa

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
rm(efi_all)
gc()



# Variable year lookback
efi_all = efi_all_ac

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



