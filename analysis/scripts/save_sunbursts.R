library(sunburstR)
library(here)
library(readr)
library(dplyr)
library(stringr)
library(genieBPC)
library(magrittr)
library(glue)
library(cli)

purrr::walk(.x = here("R", dir(here("R"))),
            .f = source)

data_list <- readRDS(
  file = here("data-raw", "genieBPC-style", "data_list.rds")
)

# As usual this function is a hive of bugs:
# create_analytic_cohort(data_synapse = data_list$Prostate_v1.2)
dl_renamed <- rename_dl_for_sunburst(data_list)
data_list$Prostate_v1.2 <- data_list$Prostate_v1.2[c("pt_char",
                                                     "ca_dx_index",
                                                     "ca_drugs",
                                                     "cpt")]

dft_trt_hist_full <- drug_regimen_sunburst(
  data_synapse = data_list$Prostate_v1.2,
  data_cohort = dl_renamed,
  max_n_regimens = 3)
dft_trt_hist_full <- dft_trt_hist_full$treatment_history


sun_wrap <- function(sun_dat, seed) {
  set.seed(seed)
  pal <- make_sun_pal(nrow(sun_dat))
  js <- sunburstR::sunburst(
    sun_dat,
    legend = F,
    count = T,
    colors = pal
  )
  return(js)
}

js_sun_full <- sun_wrap(dft_trt_hist_full, seed = 838)
saveRDS(file = here("data", "sunburst_plots", "full.rds"),
        object = js_sun_full)






# Need to pull some data to filter this second one:
read_wrap <- function(p) {
  read_csv(file = here("data-raw", p), show_col_types = F)
}

dft_ca_ind <- read_wrap("cancer_level_dataset_index.csv")
dft_med_onc <- read_wrap("med_onc_note_level_dataset.csv")

crpc_times <- make_cast_status_block(
  read_wrap("med_onc_note_level_dataset.csv"),
  read_wrap("cancer_level_dataset_index.csv"),
  remove_hspc_after_crpc = T) 
crpc_times %<>%
  filter(md_cast_status_f %in% "Castrate-Resistant") %>% 
  select(record_id, tt_y = dx_block_start) %>%
  mutate(tt_d = tt_y*365.25)


dl_crpc <- filter_dl_by_pt(
  data_list, 
  prog_timing = crpc_times
)


# Repeat the sunburst code:
dft_trt_hist_crpc <- drug_regimen_sunburst(
  data_synapse = data_list$Prostate_v1.2,
  data_cohort = dl_crpc,
  max_n_regimens = 3)
dft_trt_hist_crpc <- dft_trt_hist_crpc$treatment_history

js_sun_crpc <- sun_wrap(dft_trt_hist_crpc, seed = 8988)

saveRDS(file = here("data", "sunburst_plots", "crpc.rds"),
        object = js_sun_crpc)








dft_drug <- readr::read_csv(here("data", "drug.csv"))

# Using my drugs dataset first just to get a rough idea on the right answers:
# dft_abi_enza_doce <- 

get_limited_drug_data <- function(drug_dat, drug_str, warn = T, factorize = T) {
  drug_dat %<>%
    mutate(drug = str_replace(drug, "\\(.*\\)", ""))
  
  if (any(!(drug_str %in% unique(drug_dat$drug))) & warn) {
    unobserved_drugs <- drug_str[!(drug_str %in% unique(drug_dat$drug))]
    cli::cli_alert_danger(
      glue(
        "Some inputs not found in drug_dat: {paste(unobserved_drugs, collapse = ', ')}"
      )
    )
  }
  
  drug_dat %<>%
    filter(drug %in% drug_str) %>%
    group_by(record_id, ca_seq, regimen_number) %>%
    summarize(
      regimen_drugs = paste0(sort(drug), collapse = ", "),
      .groups = "drop"
    ) 
  
  if (factorize) {
    drug_dat %<>% mutate(regimen_drugs = factor(regimen_drugs)) 
  }
  
  drug_dat %<>% arrange(record_id, ca_seq, regimen_number) 
  
  return(drug_dat)
  
}

dft_abi_enza_doce <- get_limited_drug_data(
  dft_drug, 
  drug_str = c(
    "Abiraterone Acetate", 
    "Docetaxel",
    "Enzalutamide"
  )
  
)

dft_drug %>% 
  filter(str_detect(drug, "(Abiraterone|Enzalutamide|Docetaxel)")) %>%
  pull(drug) %>% unique
  group_by(record_id, ca_seq, regimen_number) %>%
  summarize(
    reg_cat = case_when(
      any(str_detect(drug, "(Abira|Enza)")) & any(str_detect(drug, "Doce")) ~ "Abi/Enza, Docetaxel",
      any(str_detect(drug, "(Abira|Enza)")) ~ "Abi/Enza",
      any(str_detect(drug, "(Doce)")) ~ "Docetaxel",
      T ~ "Error"
    ),
    .groups = "drop"
  ) %>%
  mutate(reg_cat = factor(reg_cat)) %>%
  arrange(record_id, ca_seq, regimen_number) 

dft_abi_enza_doce_trt_hist <- make_sunburst_input(
  dat = dft_abi_enza_doce,
  var = "reg_cat",
  order_var = "regimen_number",
  max_depth = NULL
)

js_sun_aed <- sun_wrap(dft_abi_enza_doce_trt_hist, seed = 94)
saveRDS(file = here("data", "sunburst_plots", "abi_enza_doce.rds"),
        object = js_sun_aed)


# Same thing but remove the duplicates, e.g. if a participant
#   has Enza followed by Enza - who cares?
dft_abi_enza_doce_trt_hist_no_dupes <- make_sunburst_input(
  dat = dft_abi_enza_doce,
  var = "reg_cat",
  order_var = "regimen_number",
  max_depth = NULL,
  remove_dupes = T
)

js_sun_aed_no_dupes <- sun_wrap(
  dft_abi_enza_doce_trt_hist_no_dupes, 
  seed = 94
)
saveRDS(
  file = here("data", "sunburst_plots", "abi_enza_doce_no_dupes.rds"),
  object = js_sun_aed_no_dupes
)





