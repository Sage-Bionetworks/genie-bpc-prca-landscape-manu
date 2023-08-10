# Description:  Filters the data to only index cancer rows for applicable datasets.
#.  We programatically check some basic assumptions about the data and throw errors
#.  if something looks off.
#   Additional filters may be added later on.

library(purrr); library(fs); library(here)
# Load all helper functions
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_pt <- readr::read_csv(here('data-raw', 'patient_level_dataset.csv'))
dft_ca_ind <- readr::read_csv(here('data-raw', 'cancer_level_dataset_index.csv'))
dft_cpt <- readr::read_csv(here('data-raw', 'cancer_panel_test_level_dataset.csv'))
dft_img <- readr::read_csv(here('data-raw', 'imaging_level_dataset.csv'))
dft_med_onc <- readr::read_csv(here('data-raw', 'med_onc_note_level_dataset.csv'))
dft_path <- readr::read_csv(here('data-raw', 'pathology_report_level_dataset.csv'))
dft_reg <- readr::read_csv(here('data-raw', 'regimen_cancer_level_dataset.csv'))
dft_tm <- readr::read_csv(here('data-raw', 'tm_level_dataset.csv'))

fs::dir_create(here('data', 'clin'))


# Assumption checking:
if ((dft_pt$record_id %>% duplicated %>% any)) {
  cli_abort("Duplicated records in patient level dataset.")
}

if (any(dft_pt$naaccr_sex_code != "Male")) {
  cli_alert_danger("Non-male sex detected for at least one participant (unexpectedly for Prostate cancer)")
}
# The prostate cancer data does not have duplicate rows in the cancer index data:
if (any(pull(count(dft_ca_ind, record_id), n) > 1, na.rm = T)) {
  cli_abort("Duplicate record IDs in index cancer data - need to fix.")
}


