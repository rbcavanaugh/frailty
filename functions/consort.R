library(grid)
library(glue)
library(consort)

signif_transformer <- function() {
    function(text, envir) {
        x <- identity_transformer(text, envir)
        if (is.numeric(x)) {
            prettyNum(x, big.mark = ",")
        } else {
            x
        }
    }
}
glue_mark <- function(..., .envir = parent.frame()) {
    glue(..., .transformer = signif_transformer(), .envir = .envir)
}

# Might want to change some settings
options(txt_gp = gpar(cex = 0.8))

aou = c(413360, 287012, 214375, 159721)
pmtx = c(34808145, 28076893, 9411949, 5289349)
ukbb = c(10, 6, 4, 1 )
thin = c(20, 18, 10, 5 )
emis = c(4, 3, 2, 1 )

df = tibble(
    n = c(pmtx,aou,  ukbb, thin, emis),
    source = rep(c("pmtx", "aou",  "ukbb", "thin", "emis"), each = 4)
) %>%
    group_by(source) %>%
    mutate(side = lag(n)-n)


txt0 <- glue_mark("Total number of participants in dataset
                  \u2022 Pharmetrics+ n={df[[1,1]]}
                  \u2022 All of Us n={df[[5,1]]}
                  \u2022 UK BioBank n={df[[9,1]]}
                  \u2022 IMRD THIN n={df[[13,1]]}
                  \u2022 IMRD EMIS n={df[[17,1]]}")

txt1 <- glue_mark("Consented to EHR data
                  \u2022 Pharmetrics+ n={df[[2,1]]}
                  \u2022 All of Us n={df[[6,1]]}
                  \u2022 UK BioBank n={df[[10,1]]}
                  \u2022 IMRD THIN n={df[[14,1]]}
                  \u2022 IMRD EMIS n={df[[18,1]]}")

txt1_side <- glue_mark("Excluded: No EHR/Claims Records
                       \u2022 Pharmetrics+ n={df[[2,3]]}
                       \u2022 All of Us n={df[[6,3]]}
                       \u2022 UK BioBank n={df[[10,3]]}
                       \u2022 IMRD THIN n={df[[14,3]]}
                       \u2022 IMRD EMIS n={df[[18,3]]}")

txt2 <- glue_mark("1-year of observation after index date
                  \u2022 Pharmetrics+ n={df[[3,1]]}
                  \u2022 All of Us n={df[[7,1]]}
                  \u2022 UK BioBank n={df[[11,1]]}
                  \u2022 IMRD THIN n={df[[15,1]]}
                  \u2022 IMRD EMIS n={df[[19,1]]}")

txt2_side <- glue_mark("Excluded: <1 year data available
                      \u2022 Pharmetrics+ n={df[[3,3]]}
                      \u2022 All of Us n={df[[7,3]]}
                      \u2022 UK BioBank n={df[[11,3]]}
                      \u2022 IMRD THIN n={df[[15,3]]}
                      \u2022 IMRD EMIS n={df[[19,3]]}")

txt3 <- glue_mark("40 years or older
                  \u2022 Pharmetrics+ n={df[[4,1]]}
                  \u2022 All of Us n={df[[8,1]]}
                  \u2022 UK BioBank n={df[[12,1]]}
                  \u2022 IMRD THIN n={df[[16,1]]}
                  \u2022 IMRD EMIS n={df[[20,1]]}")


txt3_side <- glue_mark("Excluded: < 40 years old
                       \u2022 Pharmetrics+ n={df[[4,3]]}
                       \u2022 All of Us n={df[[8,3]]}
                       \u2022 UK BioBank n={df[[12,3]]}
                       \u2022 IMRD THIN n={df[[16,3]]}
                       \u2022 IMRD EMIS n={df[[20,3]]}")


txt4 <- glue_mark("Final Cohort
                  \u2022 Pharmetrics+ n={df[[4,1]]}
                  \u2022 All of Us n={df[[8,1]]}
                  \u2022 UK BioBank n={df[[12,1]]}
                  \u2022 IMRD THIN n={df[[16,1]]}
                  \u2022 IMRD EMIS n={df[[20,1]]}")


# supports pipeline operator
g <- add_box(txt = txt0) |>
    add_side_box(txt = txt1_side) |>
    add_box(txt = txt1) |>
    add_side_box(txt = txt2_side) |>
    add_box(txt = txt2) |>
    add_side_box(txt = txt3_side) |>
    add_box(txt = txt3) |>
    add_box(txt = txt4)


plot(g)
png("output/consort_aou.png", width = 29,
    height = 21, res = 300, units = "cm", type = "cairo")
plot(g)
dev.off()

