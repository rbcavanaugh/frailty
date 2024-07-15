
# The basics is the first survey.
# So I'm going to assume that when people fill out the basics survey this is the
# first date to start the clock on the AoU year
survey_dates = tbl(con, "ds_survey") %>%
    filter(survey == "The Basics") %>%
    group_by(person_id) %>%
    filter(survey_datetime == min(survey_datetime)) %>%
    distinct(person_id, survey_datetime) %>%
    mutate(survey_date = as.Date(survey_datetime))


demo <- tbl(con, "cb_search_person") %>%
    filter(has_ehr_data == 1) %>%
    # just hold on to all the unique ids for merging
    distinct(person_id, sex_at_birth, dob)  %>%
    # combine with survey dates
    inner_join(survey_dates, by = "person_id") %>%
    select(person_id, sex_at_birth, dob, survey_date) %>%
    mutate(index_date = !!CDMConnector::dateadd("survey_date", 1, "year")) %>%
    mutate(age = !!CDMConnector::datediff("dob","index_date","year")) %>%
    filter(age >= 40 & age <= 100) %>%
    select(person_id,
           gender_source_value = sex_at_birth,
           age,
           visit_lookback_date = survey_date,
           index_date) %>%
    mutate(
        is_female = ifelse(gender_source_value == "Male", 0, 1),
        age_group = cut(age,
                        breaks = c(40,  45,  50,  55, 60,  65,  70,  75,  80,  100),
                        right = FALSE, include.lowest = TRUE),
    ) %>%
    select(person_id, is_female, age_group, visit_lookback_date, index_date)

demo_c = collect(demo)
# print(nrow(demo_c))
# #write_csv(demo_c, "cohort.csv")
# sample_size = nrow(demo_c)
# options(scipen = 999)
# demo_summary = demo_c %>%
#     count(age_group, is_female) %>%
#     group_by(age_group, is_female) %>%
#     mutate(percent_group = round(n/!!sample_size, 3)) %>%
#     drop_na(age_group)
# sum(demo_summary$n) == nrow(demo_c)
# write_csv(demo_summary, paste(Sys.Date(), "AoU_cohort_summary.csv", sep = "-"))
