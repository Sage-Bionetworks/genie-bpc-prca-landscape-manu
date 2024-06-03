# Create some basic plots, KM curves, truncation tests, etc.
# Don't want these to clutter up the report too much.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) # also loads lots of packages.

fs::dir_create(
  here('data', 'survival')
)

read_wrap_clin <- function(p) {
  read_rds(file = here("data", 'clin', p))
}

dft_pt <- read_wrap_clin("dft_pt.rds")
dft_ca_ind <- read_wrap_clin("dft_ca_ind.rds")
# we're taking the augmented version, which has TMB columns added.  The existing info is the same.
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")


dft_first_cpt <- get_first_cpt(
  ca_ind_dat = dft_ca_ind,
  cpt_dat = dft_cpt
)

dft_surv_dx <- dft_ca_ind %>%
  left_join(., dft_first_cpt, by = c("record_id", "ca_seq")) %>%
  select(
    record_id, ca_seq, stage_dx_iv, dx_cpt_rep_yrs,
    os_dx_status, tt_os_dx_yrs
  ) 

dft_surv_dx %<>% 
  remove_trunc_gte_event(
    trunc_var = 'dx_cpt_rep_yrs',
    event_var = 'tt_os_dx_yrs'
  )

surv_obj_os_dx <- with(
  dft_surv_dx,
  Surv(
    time = dx_cpt_rep_yrs,
    time2 = tt_os_dx_yrs,
    event = os_dx_status
  )
)

gg_os_dx_stage <- plot_one_survfit(
  dat = dft_surv_dx,
  surv_form = surv_obj_os_dx ~ stage_dx_iv,
  plot_title = "OS from diagnosis",
  plot_subtitle = "Adjusted for (independent) delayed entry",
  x_exp = 0.1
)

readr::write_rds(
  x = gg_os_dx_stage,
  file = here('data', 'survival', 'os_from_dx_by_stage.rds')
)
