# Description: Top level workflow for the project.  Can be converted to a 
#   cleaner workflow later on.
# Author: Alex Paynter

library(purrr)
library(here)
library(fs)

purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'create_drug_dat.R'))
source(here('analysis', 'script', 'save_sunbursts.R'))

rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca_manu.Rmd')
)

source(here('analysis', 'script', 'upload_outputs_synapse.R'))
