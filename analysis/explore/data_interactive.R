# Description: A sandbox for looking at and playing with the datasets.
#   This script will never be a part of the final analysis workflow.
# Author: Alex Paynter

library(readr)
library(here)
library(dplyr)
library(magrittr)

read_wrap <- function(p) {
  read_csv(file = here("data-raw", p), show_col_types = F)
}

dft_ca_radtx <- read_wrap("ca_radtx_dataset.csv")
dft_ca_ind <- read_wrap("cancer_level_dataset_index.csv")
dft_ca_nonind <- read_wrap("cancer_level_dataset_non_index.csv")
dft_ca_panel <- read_wrap("cancer_panel_test_level_dataset.csv")

dft_img <- read_wrap("imaging_level_dataset.csv")
dft_med_onc <- read_wrap("med_onc_note_level_dataset.csv")
dft_path <- read_wrap("pathology_report_level_dataset.csv")

dft_pt <- read_wrap("patient_level_dataset.csv")
dft_regimen <- read_wrap("regimen_cancer_level_dataset.csv")
dft_tm <- read_wrap("tm_level_dataset.csv")

glimpse(dft_ca_radtx)
glimpse(dft_ca_ind)
glimpse(dft_ca_panel)
glimpse(dft_img)
glimpse(dft_med_onc)
glimpse(dft_path)
glimpse(dft_regimen)
glimpse(dft_tm_level_dataset)


library(gt)
library(gtsummary)

if ((dft_pt$record_id %>% duplicated %>% any)) {
  stop("Duplicated records in patient level dataset.")
}


if (any(dft_pt$naaccr_sex_code != "Male")) {
  warning("Non-male sex detected for at least one participant (unexpectedly for Prostate cancer)")
}

dft_pt_demo_sub <- dft_pt %>%
  mutate(
    `Race (primary)` = format_ptlevel_naaccr_race_code_primary(
      naaccr_race_code_primary
    ),
    `Ethnicity` = format_ptlevel_naaccr_ethnicity_code(
      naaccr_ethnicity_code
    )) %>%
  select(Institution = institution,
         `Race (primary)`,
         `Ethnicity`)





  gtsummary::tbl_summary(data = .)
  