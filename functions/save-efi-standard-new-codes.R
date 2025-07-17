library(here)
library(tidyverse)

df <- read_csv(here("source-data", "2025-07-16_efi_possibly_eq.csv"))

write_rds(df, "data/efi_possibly_eq.rds")
