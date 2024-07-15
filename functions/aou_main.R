#install.packages("pak")
#pak::pak(c("allofus", "tidyverse", "roux-ohdsi/aouFI", "lubridate", "DBI", "CDMConnector"))

library(allofus)
library(tidyverse)
library(CDMConnector)
library(lubridate)
library(DBI)
library(aouFI)

con <- aou_connect(quiet = TRUE)


# source cohort
source(here::here("functions", "01-cohort-aou.R"))
# source aou helper functions
source(here::here("functions", "aou-helpers.R"))

################################################################################
############################ VAFI ##############################################
################################################################################

# gets vafi concepts to a tbl object
fi_id = aouFI::fi_indices %>% filter(fi == "vafi") %>% pull(concept_id)
vafi_tbl = tbl(con, "concept") %>%
    filter(concept_id %in% fi_id) %>%
    select(concept_id)
tally(vafi_tbl)

# run omop2fi for vafi
vafi <- omop2fi(con = con,
                index = "vafi",
                collect = FALSE,
                .data_search = demo,
                search_person_id = "person_id",
                search_start_date = "visit_lookback_date",
                search_end_date = "index_date",
                keep_columns = c("age_group", "is_female"),
                unique_categories = TRUE,
                concept_location = vafi_tbl,
                join_now = FALSE
)

# rejoin with the categories for summarizing. this step is done all in one with P+
j = aouFI::vafi_rev %>% mutate(concept_id = as.integer(concept_id))

t = vafi %>%
    select(person_id, age_group, is_female, concept_id, person_start_date, person_end_date) %>%
    collect() %>%
    left_join(j, by = "concept_id", relationship = "many-to-many") %>%
    select(-concept_id) %>%
    distinct() %>%
    drop_na(age_group)

# vector of vafi categories
vafi_cats = aouFI::fi_indices %>% filter(fi == "vafi") %>% distinct(category) %>% pull(category)
# summarize the vafi scores with the robust / 0 data
vafi_all <- fi_with_robust(t, demo_c, 31, 0.11, 0.21)
# summarize by category
vafi_cat_summary = summarize_cats(
    t,
    cohort = demo_c, cats = vafi_cats
) %>% arrange(category, age_group, is_female) %>%
    drop_na() %>%
    mutate(count = ifelse(count < 20, 0, count),
           percent = ifelse(count < 20, 0, percent))
# summarize overall
# summarize overall
vafi_summary <- summarize_fi(vafi_all) %>% drop_na()


t2 = summarize_cats(
    t,
    cohort = demo_c, cats = vafi_cats
) %>% arrange(category, age_group, is_female) %>%
    drop_na()

vafi_summary %>%
    rowwise() %>%
    mutate(total = sum(prefrail, frail, robust),
           total2 = sum(n_prefrail, n_frail, n_robust)/N)

# quick plot to check.
vafi_summary %>%
    pivot_longer(cols = N:robust) %>%
    filter(name %in% c("prefrail", "frail", "robust")) %>%
    mutate(is_female = as.factor(is_female),
           name = factor(name, levels = c("robust", "prefrail", "frail"))) %>%
    ggplot(aes(x = age_group, y = value, color = is_female, group = is_female)) +
    geom_point() +
    geom_line() +
    facet_wrap(~name) +
    theme_grey(base_size = 20) +
    theme(axis.text.x =  element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)

################################################################################
############################ VAFI ##############################################
################################################################################

