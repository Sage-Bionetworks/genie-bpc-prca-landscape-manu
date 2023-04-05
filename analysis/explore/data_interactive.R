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

dft_regimen


# Exercise to get to know the regimens datasets:

# linkage notes:
# regimens can be linked with cancer dx data by 
#   cohort, record_id and ca_seq

library(tidyr)

dft_ca_nonind %>% glimpse


ca_comb <- dplyr::bind_rows(
  (dft_ca_ind %>% 
    select(record_id, ca_seq) %>%
    mutate(index_ca = T)),
  (dft_ca_nonind %>%
     select(record_id, ca_seq) %>%
     mutate(index_ca = F))
)

dft_reg_aug <- dft_regimen %>%
  left_join(., ca_comb, by = c("record_id", "ca_seq"))

# for now we just want a list of the drugs used:
dft_reg_aug %<>% 
  select(cohort, record_id, ca_seq, index_ca,
         contains("drugs_drug")) %>%
  pivot_longer(cols = -c(cohort, record_id, ca_seq, index_ca),
               names_to = "drug_num",
               values_to = "drug_name") %>%
  mutate(
    drug_num = stringr::str_replace(drug_num,
                                    "drugs_drug_",
                                    ""),
    drug_num = readr::parse_number(drug_num)
  ) 

dft_reg_aug %>%
  filter(!is.na(drug_name)) %>%
  group_by(drug_name, cohort, index_ca) %>%
  summarize(n = n())


  

names(dft_ca_nonind) %>% sort

dft_regimen %>% 
  select(cohort, record_id, ca_seq, contains("drugs_drug")) %>%
  pivot_longer(cols = -c(cohort, record_id, ca_seq))

