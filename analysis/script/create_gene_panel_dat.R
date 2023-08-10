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

saveRDS(
  object = gp_all,
  file = here('data', 'genomic', 'gene_panel_all.rds')
)



# Merge in the sample data.