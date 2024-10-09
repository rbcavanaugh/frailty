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


n1 = 413360; n2 = 287012; n3 = 214375; n4 = 159721
e1 = n1-n2
e2 = n2-n3
e3 = n3-n4

txt0 <- glue_mark("Total number of participants with basics survey (n={n1})")
txt1 <- glue_mark("Consented to EHR data (n={n2}")
txt1_side <- glue_mark("Excluded (n={e1})\n\u2022 EHR not provided")
txt2 <- glue_mark("1-year of data available post-enrollment (n={n3})")
txt2_side <- glue_mark("Excluded (n={e2})\n\u2022 <1 year data available")
txt3 <- glue_mark("40 years or older (n={n4})")
txt3_side <- glue_mark("Excluded (n={e3})\n\u2022 < 40 years old")
txt4 <- glue_mark("Final Cohort n={n4}")


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

