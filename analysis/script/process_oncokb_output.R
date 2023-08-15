# Description:  Assess the impact of oncoKB filtering, save files with
#  only the variants which pass an oncoKB pass.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_mut_onco <- readr::read_tsv(
  here('data', 'genomic', 'mut_onco.txt'),
  show_col_types = F
)
dft_cna_onco <- readr::read_tsv(
  here('data', 'genomic', 'cna_long_selected_onco.txt'),
  show_col_types = F
)
dft_fus_onco <- readr::read_tsv(
  here('data', 'genomic', 'fus_onco.txt'),
  show_col_types = F
)

# For CNAs we focus only on the highly amplified cases, taking the decisions from the breast manuscript as a good starting point if nothing else.
dft_cna_raw <- readr::read_tsv(
  here('data', 'genomic', 'cna_long_selected.txt'),
  show_col_types = F
) 
dft_cna_onco <- bind_cols(
  dft_cna_onco,
  select(dft_cna_raw, value)
) %>%
  filter(value %in% 2) # highly amplified

anno_msg_help <- function(dat) {
  dat_name <- deparse(substitute(dat))
  
  tab <- tabyl(dat, ANNOTATED) %>%
    filter(ANNOTATED) # T/F so this grabs the annotated pct line.
  
  cli::cli_alert_info(
    text = glue("{dat_name}: Of the {tab$n} rows, {round(tab$percent*100,1)}% were annotated.")
  )
}
# Just some print outs for the analyst - hoping for near 100% in all:
anno_msg_help(dft_mut_onco)
anno_msg_help(dft_cna_onco)
anno_msg_help(dft_fus_onco)

onco_count_help <- function(dat, label) {
  tabyl(dat, ONCOGENIC) %>%
    mutate(type = label) %>%
    select(type, oncogenic = ONCOGENIC, n)
}

dft_onco_impact <- bind_rows(
  onco_count_help(dft_mut_onco, "Mutation"),
  onco_count_help(dft_cna_onco, "CNA"),
  onco_count_help(dft_fus_onco, "Fusion")
)

dft_onco_impact

lev_onco <- c("Oncogenic", "Likely Oncogenic",
              "Likely Neutral",
              "Inconclusive", "Unknown")

dft_onco_impact %<>%
  mutate(
    oncogenic = factor(oncogenic, levels = lev_onco),
    type = fct_inorder(type)
  )

readr::write_rds(
  x = dft_onco_impact,
  file = here('data', 'genomic', 'oncokb_impact.rds')
)






# Assess the oncoKB impact on individual mutations

dft_cpt <- readr::read_rds(
  here('data', 'clin', 'dft_cpt.rds')
)
dft_alterations <- dft_cpt %>%
  select(
    cpt_genie_sample_id, record_id, ca_seq, cpt_seq_assay_id, 
    contains("sample_type")
  )
dft_gp_all <- readr::read_rds(
  here('data', 'genomic', 'gene_panel_all.rds')
)

dft_alterations %<>%
  left_join(., dft_gp_all, by = "cpt_seq_assay_id",
            relationship = "many-to-many")

dft_alterations %<>%
  group_by(hugo) %>%
  mutate(n_tested_this_gene = n()) %>%
  ungroup(.)

dft_mut_onco %>% glimpse
dft_mut_onco_sub <- dft_mut_onco %>% 
  mutate(
    onco_filter = if_else(
      ONCOGENIC %in% lev_onco[1:2],
      T,
      F, 
      F)
  ) %>%
  select(
    cpt_genie_sample_id = Tumor_Sample_Barcode,
    hugo = Hugo_Symbol, 
    # Consequence, Variant_Classification, Variant_Type,
    # GENE_IN_ONCOKB, VARIANT_IN_ONCOKB,
    onco_filter
  )

# For multiple alterations, we compress them into one line:
dft_mut_onco_sub %<>% 
  group_by(cpt_genie_sample_id, hugo) %>%
  mutate(onco_filter = any(onco_filter)) %>%
  slice(1) %>%
  ungroup(.)

dft_mut_onco_raw <- dft_mut_onco_sub %>%
  mutate(variant_mut_raw = 1) %>%
  select(-onco_filter)

dft_mut_onco_filt <- dft_mut_onco_sub %>%
  filter(onco_filter) %>%
  mutate(variant_mut_okb = 1) %>%
  select(-onco_filter)

dft_alterations %<>%
  left_join(
    ., 
    dft_mut_onco_raw, 
    by = c("cpt_genie_sample_id", "hugo"),
    relationship = "one-to-many"
  ) %>%
  left_join(
    ., 
    dft_mut_onco_filt, 
    by = c("cpt_genie_sample_id", "hugo"),
    relationship = "one-to-many"
  ) 

dft_alterations %<>%
  mutate(
    across(
      .cols = c("variant_mut_raw", "variant_mut_okb"),
      .fns = (function(x) if_else(is.na(x), 0, x))
    )
  )

dft_alterations %<>%
  mutate(
    sum_alt = variant_mut_raw + variant_mut_okb
  ) %>%
  filter(sum_alt >= 1) %>%
  select(-sum_alt)

#dft_alterations$variant_mut_raw %>% sum
#dft_alterations$variant_mut_okb %>% sum


dft_alterations %<>%
  mutate(alteration_type = "Mutation") %>%
  pivot_longer(
    cols = contains("variant"),
    names_to = "filter",
    values_to = "alteration"
  ) 

dft_alterations %<>%
  mutate(
    filter = if_else(
      str_detect(filter, "raw"),
      "Raw",
      "OncoKB"
    )
  ) 


dft_alterations %<>% 
  group_by(hugo, filter) %>%
  summarize(
    n_altered = sum(alteration),
    n_tested_this_gene = first(n_tested_this_gene),
    .groups = "drop"
  ) %>%
  mutate(
    pct_altered = n_altered / n_tested_this_gene
  )

readr::write_rds(
  x = dft_alterations,
  file = here('data', 'genomic', 'oncokb_impact_by_gene.rds')
)

               
  



  
  





