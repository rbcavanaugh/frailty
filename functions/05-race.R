# race
#
#

library(tidyverse)
library(CDMConnector)
library(glue)
library(here)
library(allofus)
#library(aouFI)

# set to pharmetrics or allofus.
data_source = "allofus"
dbms = "bigquery" # bigrquery DBI connection doesn't hold the information the same way.

# ============================================================================
# ################################ SETUP #######################################
# ============================================================================

con <- allofus::aou_connect(quiet = TRUE)

# get cohort
source(here::here("functions", "01-cohort-aou.R"))
cohort_all = demo
# get pp
# takes a hot second

race_eth = tbl(con, "person") |> select(person_id, race_concept_id, ethnicity_concept_id) |>
    left_join(tbl(con, "concept") |> select(race_concept_id = concept_id, race = concept_name), by = "race_concept_id") |>
    left_join(tbl(con, "concept") |> select(ethnicity_concept_id = concept_id, ethnicity = concept_name), by = "ethnicity_concept_id")

cohort_demo = cohort_all |>
    left_join(race_eth, by = "person_id") |>
    select(person_id, is_female, age_group, race, ethnicity)

tally(cohort_demo)




