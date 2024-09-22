
library(tidyverse)
library(ohdsilab)
library(DBI)
library(DatabaseConnector)
library(CDMConnector)

source(here::here("functions", "connection_setup.R"))

# tbl(con, inDatabaseSchema(my_schema, "vafi_rev")) |> collect() -> vafi_rev
# vafi_rev = vafi_rev %>% mutate(concept_id = as.integer(concept_id))
# usethis::use_data(vafi_rev, overwrite = TRUE)

# Summary Functions. now in separate script
source(here::here("functions", "summary_functions.R"))

# cdm_schema is the omop db
# my_schema is the user write schema

# ============================================================================
# COHORT
cohort <- tbl(con, inDatabaseSchema(my_schema, "frailty_cohort_clean"))
tally(cohort)
# 5292854
# ============================================================================
# ICD VERSION

dat <- icd_fi()

dat2 = dat |>
    distinct(person_id, age_group, is_female, category = deficit) |>
    mutate(score = 1)

dat %>% distinct(deficit) %>% collect() -> u

#icd_c = dbi_collect(dat2)
# add robust individuals back
icd_all <- fi_with_robust(fi_query = dat2,
                          cohort = cohort,
                          denominator = 31, lb = 0.11, ub = 0.21)

# ============================================================================
# VAFI OMOP VERSION

vafi <- aouFI::omop2fi(con = con,
                       schema = cdm_schema,
                       index = "vafi",
                       .data_search = cohort,
                       search_person_id = "person_id",
                       search_start_date = "visit_lookback_date",
                       search_end_date = "index_date",
                       keep_columns = c("age_group", "is_female"),
                       collect = FALSE,
                       unique_categories = TRUE,
                       concept_location = tbl(con, inDatabaseSchema(my_schema, "vafi_rev"))
) |>
    distinct(person_id, age_group, is_female, score, category)


# save result of query as intermediate step #2
# CDMConnector::computeQuery(vafi, "vafi_fi",
#                            temporary = TRUE,
#                            schema = my_schema, overwrite = TRUE)

# add robust individuals back
vafi_all <- fi_with_robust(fi_query = vafi,
                           cohort = cohort,
                           denominator = 31, lb = 0.11, ub = 0.21)
# ============================================================================
# PLOT DIFFERENCES

icd_cats = dat2 |>
    summarize(sum_score = sum(score), .by = category) |>
    collect()

omop_cats = vafi |>
    summarize(sum_score = sum(score), .by = category) |>
    collect()

df_plot = left_join(omop_cats, icd_cats, by = c("category")) #%>% filter(category != "HTN")

write.csv(df_plot, "output/data_for_icc.csv")

plotly::ggplotly(df_plot |>
                     ggplot(aes(x = sum_score.x, y = sum_score.y, color = category)) +
                     geom_point() +
                     geom_abline() +
                     labs(x = "omop", y = "icd"))

df_plot |>
    mutate(sum_score.x = sum_score.x/5292854,
           sum_score.y = sum_score.y/5292854) %>%
    ggplot(aes(x = sum_score.x, y = sum_score.y)) +
    geom_point() +
    geom_abline() +
    labs(x = "OMOP Concept ID Prevalence", y = "ICD-10 Prevalence") +
    theme_minimal(base_size = 16) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 0.5)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.5))

ggsave("output/supplement_p1.png", width = 8, height = 8)


df_plot_noHTN = left_join(omop_cats, icd_cats, by = c("category")) %>% filter(category != "HTN")

cor(df_plot$sum_score.x, df_plot$sum_score.y, use = "complete.obs")
cor(df_plot_noHTN$sum_score.x, df_plot_noHTN$sum_score.y, use = "complete.obs")

icc1 = irr::icc(df_plot %>% select(-category), model = "oneway", type = "agreement")
icc2 = irr::icc(df_plot_noHTN %>% select(-category), model = "oneway", type = "agreement")

