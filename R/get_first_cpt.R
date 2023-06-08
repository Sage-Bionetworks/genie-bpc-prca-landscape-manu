get_first_cpt <- function(ca_ind_dat) {
  rtn <- ca_ind_dat %>% 
    select(record_id, ca_seq) %>%
    left_join(., dft_cpt, by = c("record_id", "ca_seq")) %>%
    arrange(cpt_number) %>%
    group_by(record_id) %>%
    slice(1) %>%
    ungroup() %>%
    select(record_id, ca_seq, dx_cpt_rep_yrs)
    
  return(rtn)
}
