


library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) # also loads lots of packages.

read_wrap_clin <- function(p) {read_rds(file = here("data", 'clin', p))}
dft_pt <- read_wrap_clin("dft_pt.rds")
dft_ca_ind <- read_wrap_clin("dft_ca_ind.rds")
# we're taking the augmented version, which has TMB columns added.  The existing info is the same.
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")


dft_surv_met <- dft_ca_ind %>%
  filter(!is.na(os_adv_status)) %>%
  left_join(., dft_first_cpt, by = c("record_id", "ca_seq")) %>%
  select(
    record_id, ca_seq, dx_cpt_rep_yrs,
    os_adv_status, tt_os_adv_yrs
  )


check_unique_key(dft_surv_met, record_id, ca_seq)

check_unique_key <- function(
    dat,
    ...
) {
  top_count <- dat %>%
    count(..., sort = T) %>%
    slice(1)
  
  if (top_count$n > 1) {
    print(top_count)
    cli::cli_abort("Duplicate keys ^.  Top hit printed above")
    return(F)
  } else {
    return(T)
  }
}

check_unique_key(dft_surv_met, record_id, ca_seq)
    

if (
  (dft_surv_met %>% count(record_id, ca_seq, sort = T) %>% pull(n) %>% 
   max %>% is_greater_than(1))
) {
  cli_abort("Duplicate keys")
}
  

dft_met_timing <- make_dmet_status_block(ca_ind_dat = dft_ca_ind) %>% 
  filter(dmet_status %in% "Distant Metastasis") %>%
  select(record_id, ca_seq, dx_met_yrs = dx_block_start)



dft_surv_met <- left_join(dft_surv_met,
                          dft_met_timing,
                          by = c('record_id', 'ca_seq'))

dft_surv_met %>% 
  remove_trunc_gte_event(
    trunc_var = 'dx_cpt_rep_yrs',
    event_var = 'tt_os_adv_yrs'
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
  surv_form = surv_obj_os ~ stage_dx_iv,
  plot_title = "OS from diagnosis",
  plot_subtitle = "Adjusted for (independent) delayed entry"
)