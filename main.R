# Description: Top level workflow for the project.  Can be converted to a 
#   cleaner workflow later on.
# Author: Alex Paynter

library(purrr)
library(here)
library(fs)

purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

############
# Clinical #
############
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'create_drug_dat.R'))
source(here('analysis', 'script', 'save_sunbursts.R'))

rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca_manu.Rmd')
)


###########
# Genomic #
###########
# Need to do once:
# source(here('analysis', 'script', 'cna_reshape.R'))
# run annotate_oncokb.sh from the command line.  See comments on enviro vars.
# source(here('analysis', 'script', 'save_oncokb_annotated_data.R'))

# After that the files can be obtained from synid in save_oncokb_annotated_data.R



##########
# Output #
##########

source(here('analysis', 'script', 'upload_outputs_synapse.R'))
