library(sunburstR)
library(genieBPC)

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

set.seed(838)
pal_sun_full <- make_sun_pal(nrow(dft_trt_hist_full))

js_sun_full <- sunburstR::sunburst(
  dft_trt_hist_full,
  legend = F,
  count = T,
  colors = pal_sun_full
)

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
  mutate(tt_d = tt_y/365.25)


dl_crpc <- filter_dl_by_pt(data_list, prog_timing = crpc_times)


# Repeat the sunburst code:
dft_trt_hist_crpc <- drug_regimen_sunburst(
  data_synapse = data_list$Prostate_v1.2,
  data_cohort = dl_crpc,
  max_n_regimens = 3)
dft_trt_hist_crpc <- dft_trt_hist_crpc$treatment_history

set.seed(8988)
pal_sun_full <- make_sun_pal(nrow(dft_trt_hist_crpc))

js_sun_crpc <- sunburstR::sunburst(
  dft_trt_hist_crpc,
  legend = F,
  count = T,
  colors = pal_sun_full
)

saveRDS(file = here("data", "sunburst_plots", "crpc.rds"),
        object = js_sun_crpc)
