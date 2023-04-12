# Description: A sandbox for looking at and playing with the datasets.
#   This script will never be a part of the final analysis workflow.
# Author: Alex Paynter

library(cli) # prevents an error on version reliance.
library(readr)
library(vctrs)
library(rlang)
library(here)
library(dplyr)
library(tidyr)
library(magrittr)
library(janitor)
library(glue)
library(genieBPC)

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
glimpse(dft_tm)

dft_regimen %>% tabyl(redcap_ca_index) # Most (not all) associated with a BPC cancer, seems like an obvious filter.

dft_regimen %<>% filter(redcap_ca_index %in% "Yes")

dft_regimen %>% tabyl(drugs_num) # NO regimens with 5 drugs, so hopefully there isn't a ceiling on recording.

dft_regimen %>% tabyl(drugs_ct_yn) # sizable chunk of clin trial data, which will be marked as "Investigational drug".  End dates are junk data (should be missing but they're not) in those cases.

dft_regimen %>% tabyl(drugs_dc_ynu) # MOST regimens are discontinued.

dft_regimen %>% 
  arrange(desc(drugs_num)) %>%
  filter(str_detect(regimen_drugs, "Investigational Drug")) %>% 
  head(100) %>%
  pull(regimen_drugs) # Appears to be alphabetical, except that 

# get a 1-row-per-drug dataset:
dft_drug <- dft_regimen %>% 
  select(cohort:redcap_ca_index,
         drugs_drug_1:dx_drug_end_or_lastadm_int_5) %>%
  mutate(drugs_drug_5 = vec_cast(drugs_drug_5, 
                                 to = "character")) %>%
  mutate(
    across(
      .cols = drugs_startdt_int_1:dx_drug_end_or_lastadm_int_5,
      .fns = as_double
    )
  )

# split off the character values, we'll do those separately
#   to avoid casting issues.
dft_drug_char <- dft_drug %>%
  select(record_id, regimen_number, 
         drugs_drug_1: drugs_drug_5)

dft_drug %<>%
  select(-contains("drugs_drug_")) %>%
  pivot_longer(
    cols = drugs_startdt_int_1:dx_drug_end_or_lastadm_int_5,
    names_to = "var",
    values_to = "value"
  ) %>%
  # Because "drug_num" is confusing when we have "drugs_num" in 
  #   the raw data.
  # This is only an id for drug within regimen within person.
  mutate(drug_id = readr::parse_number(var),
         var = stringr::str_replace(var, "_[0-5]", "")) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  select(record_id, contains("regimen_number"), drug_id, everything()) 

# Now do it again for the cluster of character columns.
#   All the previous ones were integer/double.
dft_drug_char %<>%
  pivot_longer(
    cols = drugs_drug_1: drugs_drug_5,
    names_to = "var",
    values_to = "value"
  ) %>%
  # Because "drug_num" is confusing when we have "drugs_num" in 
  #   the raw data.
  # This is only an id for drug within regimen within person.
  mutate(drug_id = readr::parse_number(var),
         var = stringr::str_replace(var, "_[0-5]", "")) %>%
  # because there is only one column we don't need to pivot here,
  #  just rename.
  select(record_id, regimen_number, drug_id, drug = value)

dft_drug <-
  left_join(dft_drug, dft_drug_char, 
            by = c("record_id", "regimen_number", "drug_id")) %>%
  relocate(drug, .before = drugs_startdt_int)

# empty rows here have no meaning - it's just regimens with less
#   than 5 drugs which is not suprising or interesting.
dft_drug %<>%
  filter(!is.na(drug))

# Easy to count drugs now:
dfp_top_drugs <- dft_drug %>%
  group_by(drug) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

# A function that finds the top k drugs in regimens that use drug_name.
get_drug_compliments <- function(drug_dat, drug_name) {
  reg_with_drug <- drug_dat %>%
    filter(drug %in% drug_name) %>%
    select(record_id, regimen_number) %>%
    distinct(.)
  
  n_reg <- nrow(reg_with_drug)
  
  top_drug_compliments <- left_join(
    reg_with_drug, 
    drug_dat,
    by = c("record_id", "regimen_number")
  ) %>%
    filter(!(drug %in% drug_name)) %>%
    group_by(drug) %>%
    summarize(n = n(), .groups = "drop")
  
  top_drug_compliments %<>%
    arrange(desc(n)) %>%
    mutate(n_reg = n_reg,
           prop = n/n_reg)
  
  return(top_drug_compliments)
  
}

reshape_drug_compliments <- function(df_dc, k = 3) {
  df_dc %<>% arrange(desc(n))
  
  ordinal_words_vec <- c("first", "second", "third", "fourth", "fifth",
                         "sixth", "seventh", "eighth", "ninth", "tenth")
  
  df_dc %>%
    slice(1:k) %>%
    mutate(
      order = ordinal_words_vec[1:min(k, n())],
      # remove synonyms:
      drug = str_replace(drug, "\\(.*\\)", ""),
      str = glue("{drug} ({formatC(prop*100,1, format = 'f', digits = 1)}%)")
    ) %>%
    select(order, str) %>%
      pivot_wider(names_from = "order", values_from = "str")
}

# test on one drug:
get_drug_compliments(dft_drug, dfp_top_drugs[[2, "drug"]]) %>%
  reshape_drug_compliments()
dc_wrap <- function(drug_name) {
  get_drug_compliments(dft_drug, drug_name) %>%
    reshape_drug_compliments()
}
dc_wrap(dfp_top_drugs[[2, "drug"]])

# apply to all rows:
dfp_top_drugs <- dfp_top_drugs %>%
  mutate(df_top_comp = purrr::pmap(
    .l = list(drug_name = drug),
    .f = dc_wrap
  )) %>%
  tidyr::unnest(df_top_comp) %>%
  mutate(drug = str_replace(drug, "\\(.*\\)", ""))




dfp_top_drugs %>% View(.)



# Some exploration needed on the cancer level dataset too, to figure out how the primary, mets, etc are related to ca_seq.

# From Jen's email: mapping breast cancer sites.
dft_ca_ind %>%
  # variables needed for people who were stage IV at dx:
  select(record_id, 
         stage_dx_iv,
         dob_ca_dx_days, # dob_ca_dx_days appears to match jen's desc of "ca_cadx_int"
         ca_dmets_yn,
         contains("ca_first_dmets"))
  # Now something I don't get:  How can they be stage 4 at dx but not have dmets?
  
dft_ca_ind %>%
  # variables needed for people who were stage IV at dx:
  select(record_id, 
         stage_dx_iv,
         dob_ca_dx_days, # dob_ca_dx_days appears to match jen's desc of "ca_cadx_int"
         dx_to_dmets_days,
         matches("^dmets_"),
         matches("^dx_to_dmets_")) %>%
  glimpse

str_filter <- function(vec, pattern, negate = F) {
  str_ind <- str_which(vec, pattern, negate = negate)
  vec[str_ind]
}




