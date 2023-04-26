library(ggplot2)

dft_regimen <- read_wrap("regimen_cancer_level_dataset.csv")

pt_subset <- dft_pt$record_id %>% 
  unique %>% 
  sample(x = ., size = 20, replace = F)

pt_subset <- tibble(
  scrambled_id = 1:20,
  record_id = sample(pt_subset)
) %>%
  mutate(
    scrambled_id = str_pad(scrambled_id, width = 3, 
                           side = "left", pad = "0"),
    scrambled_id = paste0("s-", scrambled_id),
    scrambled_id = factor(scrambled_id)
  )

dft_reg_sub <- dft_regimen %>% 
  filter(record_id %in% pt_subset$record_id) %>%
  left_join(., pt_subset, by = "record_id") %>%
  select(-record_id)

dft_ca_ind %>%
  select(os_dx_status, tt_os_dx_yrs)

# Good examples:
dft_med_onc %>%
  select(record_id, dx_md_visit_yrs, md_pca_status) %>%
  filter(record_id %in% "GENIE-DFCI-000061") %>%
  print(n = 100)
# Two blocks:  Not stated from 0-2.54, and CRPC from 2.54 to end.

dft_med_onc %>%
  select(record_id, dx_md_visit_yrs, md_pca_status) %>%
  filter(record_id %in% "GENIE-MSK-P-0029655") %>%
  print(n = 100)
# Three blocks:  Not stated from 0-6.73, HSPC from 6.73-7.24, CRPC from 7.24 to end.

dft_cast_blocks <- dft_med_onc %>%
  select(record_id, dx_md_visit_yrs, md_pca_status) %>%
  filter(str_detect(md_pca_status, "(CRPC)|(HSPC)")) %>%
  # keep the first row or any row where the designation flips.
  group_by(record_id) %>%
  filter(1:n() == 1 | lag(md_pca_status) != md_pca_status) %>%
  ungroup() %>%
  left_join(
    ., 
    select(dft_ca_ind, record_id, os_dx_status, tt_os_dx_yrs),
    by = "record_id"
  )


# If you want to ignore declarations of hormonse sensitive prostate cancer
#   which occur after a declaration of castrate-resistant prostate cancer:
dft_cast_blocks %<>%
  group_by(record_id) %>%
  mutate(
    cast_resist = str_detect(md_pca_status, "Castrate-Resistant"),
    cast_resist_sum = cumsum(cast_resist)
  ) %>%
  # Stop once you hit the first castration resistant block.
  filter(cast_resist_sum <= 1 & (1:n() == 1 | lag(cast_resist_sum) <= 0)) %>%
  select(-cast_resist_sum, -cast_resist) %>%
  ungroup()


dft_cast_blocks %<>%
  group_by(record_id) %>%
  mutate(dx_block_end = case_when(
    # if it's the last row, the block ends at date of death.
    1:n() == n() ~ tt_os_dx_yrs,
    T ~ lead(dx_md_visit_yrs) # otherwise ends at next block.
  )) %>%
  ungroup()

first_block <- dft_cast_blocks %>%
  group_by(record_id) %>%
  arrange(dx_md_visit_yrs) %>%
  slice(1) %>%
  select(record_id, dx_block_end = dx_md_visit_yrs) %>%
  left_join(
    (dft_ca_ind %>% select(record_id, os_dx_status, tt_os_dx_yrs)),
    ., 
    by = "record_id"
  ) %>%
  # for those who had no declare prostate cancer blocks, 
  # they're always unknown
  mutate(
    dx_md_visit_yrs = 0,
    dx_block_end = if_else(is.na(dx_block_end),
                                tt_os_dx_yrs,
                                dx_block_end),
    md_pca_status = "Not yet declared"
  ) 

dft_cast_blocks <- bind_rows(
  first_block,
  dft_cast_blocks
) %>%
  arrange(record_id, dx_md_visit_yrs)


    

dft_cast_blocks %>% 
  filter(record_id %in% c("GENIE-MSK-P-0029655",
                          "GENIE-DFCI-000061"))


ggplot(filter(dft_cast_blocks, record_id %in% pt_subset),
       aes(y = record_id, yend = record_id)) + 
  geom_segment(aes(x = dx_md_visit_yrs, 
                   xend = dx_block_end, 
                   color = md_pca_status)) + 
  theme_bw() + 
  theme(legend.position = "bottom")

# Todos:
#   - Get the "make blocks" steps into a function - optional filter on the CRPC HSPC designation.
#   - Tune up the plot specs.  Consider doing a plot with X = age as well.










tabyl(md_pca_status)


data_process_swimmer <- function(ca_ind, reg, pt_selection) {
  # assign a random order to participants:
  record_id_levs = factor(sample(pt_selection))
  
  ca_ind_sub <- ca_ind %>%
    select(record_id, os_dx_status, tt_os_dx_yrs)
  
  dft_reg_sub <- dft_regimen %>% 
    filter(record_id %in% pt_selection) %>%
    mutate(record_id_f = factor(record_id, levels = record_id_levs)) %>%
    left_join(., ca_ind_sub, by = record_id)
}


library(ggplot2)



ggplot(data = dft_reg_sub) + 
  geom_segment(aes(x = dx_reg_start_int_yrs,
                   xend = dx_reg_end_all_int_yrs,
                   y = scrambled_id,
                   yend = scrambled_id)) + 
  theme_classic()
