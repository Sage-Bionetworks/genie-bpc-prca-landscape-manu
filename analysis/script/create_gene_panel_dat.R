# Description: Grab all the gene panel files and create data frames
#   to organize that information. 

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

vec_gene_panels <- fs::dir_ls(here('data-raw', 'genomic')) %>%
  str_filter(., 'data_gene_panel_.*')

gp_all <- purrr::map_dfr(
  .x = vec_gene_panels,
  .f = tidy_gene_panel
)

# for consistency with clinical data (and I like the name better)
gp_all %<>% rename(cpt_seq_assay_id = stable_id)

saveRDS(
  object = gp_all,
  file = here('data', 'genomic', 'gene_panel_all.rds')
)



gp_all %>% glimpse


# Merge in the sample data.
dft_cpt <- readr::read_rds(
  here('data', 'clin', 'dft_cpt.rds')
)

gp_sum <- dft_cpt %>% 
  select(cpt_genie_sample_id, record_id, cpt_number, 
         sample_type, cpt_seq_assay_id) %>%
  group_by(cpt_seq_assay_id) %>%
  summarize(
    n_pts = length(unique(record_id)),
    n_samples = n(),
    .groups = "drop"
  )

# Make sure we have a gene panel data file for each one found in the CPT data:
chk_gp_files <- all(gp_sum$cpt_seq_assay_id %in% unique(gp_all$cpt_seq_assay_id))
if (!chk_gp_files) {
  cli_abort("At least one gene panel found in the CPT data with no corresponding metadata file")
}

# get the number of genes in each panel, merge that in to the summary dataframe.
gp_sum <- gp_all %>%
  group_by(cpt_seq_assay_id) %>%
  summarize(
    # n() would also be fine here:
    n_genes = length(unique(hugo)),
    .groups = 'drop'
  ) %>%
  left_join(
    gp_sum,
    .,
    by = "cpt_seq_assay_id"
  )

gp_sum %<>% 
  arrange(desc(n_pts)) %>%
  mutate(
    cpt_seq_assay_id = forcats::fct_inorder(cpt_seq_assay_id)
  )
  
saveRDS(
  object = gp_sum,
  file = here('data', 'genomic', 'gene_panel_sum.rds')
)




gp_by_gene <- gp_all %>% 
  group_by(hugo) %>%
  summarize(
    n_panels = length(unique(cpt_seq_assay_id)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_panels), hugo) %>%
  mutate(
    hugo = forcats::fct_inorder(hugo)
  )

# apply the factor levels above to the "all" data:
gp_all %<>%
  mutate(
    cpt_seq_assay_id = factor(
      cpt_seq_assay_id, 
      levels = levels(gp_sum$cpt_seq_assay_id)
    ),
    hugo = factor(
      hugo, 
      levels = levels(gp_by_gene$hugo)
    )
  )


# Save the datasets.
saveRDS(
  object = gp_all,
  file = here('data', 'genomic', 'gene_panel_all.rds')
)
saveRDS(
  object = gp_sum,
  file = here('data', 'genomic', 'gene_panel_sum.rds')
)
saveRDS(
  object = gp_by_gene,
  file = here('data', 'genomic', 'gene_panel_by_gene.rds')
)

  
