library(tidyverse)

df_plot <- read_csv("output/data_for_icc.csv") %>% select(-1)

df_plot |>
     ggplot(aes(x = sum_score.x, y = sum_score.y, color = category)) +
     geom_point() +
     geom_abline() +
     labs(x = "omop", y = "icd")

icc1 = irr::icc(df_plot %>% select(-category), model = "oneway", type = "agreement")

# Single Score Intraclass Correlation
#
# Model: oneway
# Type : agreement
#
# Subjects = 31
# Raters = 2
# ICC(1) = 0.995
#
# F-Test, H0: r0 = 0 ; H1: r0 > 0
# F(30,31) = 409 , p = 5.49e-33
#
# 95%-Confidence Interval for ICC Population Values:
#     0.99 < ICC < 0.998


plot1 = df_plot |>
    mutate(sum_score.x = sum_score.x/5099699,
           sum_score.y = sum_score.y/5099699) %>%
    ggplot(aes(x = sum_score.x, y = sum_score.y)) +
    geom_point() +
    geom_abline() +
    labs(x = "OMOP Concept ID Prevalence", y = "ICD-10 Prevalence") +
    theme_minimal(base_size = 16) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 0.5)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.5))

# add icc1 1-way agreement ICC text onto the figure above and include the confidence interval
plot1 +
    annotate("text", x = 0.4, y = 0.01,
             label = "ICC(1) = 0.995; 95% CI: [0.99, 0.998]", size = 6)



ggsave("output/supplement_p1.png", width = 8, height = 8)


df_plot_noHTN = left_join(omop_cats, icd_cats, by = c("category")) %>% filter(category != "HTN")

cor(df_plot$sum_score.x, df_plot$sum_score.y, use = "complete.obs")
cor(df_plot_noHTN$sum_score.x, df_plot_noHTN$sum_score.y, use = "complete.obs")

icc2 = irr::icc(df_plot_noHTN %>% select(-category), model = "oneway", type = "agreement")

