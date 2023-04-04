# Description: A sandbox for looking at and playing with the datasets.
#   This script will never be a part of the final analysis workflow.
# Author: Alex Paynter

library(readr)
library(here)

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
dft_tm_level_dataset <- read_wrap("tm_level_dataset.csv")



