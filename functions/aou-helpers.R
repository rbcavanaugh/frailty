# The omop2fi() function only finds people who have FI scores > 0.
# Se we need to find also all the people with FI scores == 0 and add
# them back to the dataset.
# fi_query is the result of omop2fi() and
# lb is the cutoff between robust and prefrail and
# ub is the cutoff between prefrail and frail
# denominator is how many FI categories there are for the index
fi_with_robust <- function(fi_query, cohort, denominator, lb, ub){

    # find all the people in the cohort query that are not in the fi_query
    tmp = cohort |>
        anti_join(fi_query |> select(person_id), by = "person_id") |>
        select(person_id, age_group, is_female) |>
        mutate(fi = 0, frail = 0, prefrail = 0, robust = 1)

    # add them back to the FI query while calculating the person-level FI
    fi_query |>
        distinct(person_id, score, category, is_female, age_group) |>
        select(person_id, is_female, age_group, score) |>
        summarize(fi = sum(score)/denominator, .by = c(person_id, age_group, is_female)) |>
        mutate(prefrail = ifelse(fi>= lb & fi < ub, 1, 0),
               frail = ifelse(fi>= ub, 1, 0),
               robust = ifelse(fi < lb, 1, 0)) |> ungroup() |>
        bind_rows(tmp)

}

# simple function to summarize the FI scores
summarize_fi <- function(fi_query){

    fi_query |>
        summarize(N = n(),
                  n_prefrail = sum(prefrail),
                  n_frail = sum(frail),
                  n_robust = sum(robust),
                  prefrail = sum(prefrail)/n(),
                  frail = sum(frail)/n(),
                  robust = sum(robust)/n(),
                  .by = c(age_group, is_female))

}

# disorder area
summarize_cats <- function(fi_query, cohort, cats){

    #     # find all the people in the cohort query that are not in the fi_query
    #     tmp = cohort |>
    #         anti_join(fi_query |> select(person_id), by = "person_id") |>
    #         select(person_id, age_group, is_female) |>
    #         expand_grid(tibble(category = cats)) |>
    #         mutate(score = 0)

    tmp = cohort |>
        select(person_id, age_group, is_female) |>
        expand_grid(tibble(category = cats)) |>
        left_join(fi_query |> select(person_id, category, score),
                  by = c("person_id", "category")) |>
        mutate(score = ifelse(is.na(score), 0, score)) |>
        summarize(N = n(),
                  count = sum(score),
                  percent = sum(score)/N,
                  .by = c(age_group, is_female, category))

    #     # add them back to the FI query while calculating the person-level FI
    #     fi_query |> ungroup() |>
    #         distinct(person_id, score, category, is_female, age_group) |>
    #         bind_rows(tmp) |>
    #         add_count(is_female, age_group, category) |> print() |>
    #         summarize(N = n(),
    #                   n_prev = mean(n),
    #                   count = sum(score),
    #                   percent = sum(score)/N,
    #                   .by = c(age_group, is_female, category))


}
