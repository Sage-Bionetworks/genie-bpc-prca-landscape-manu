# Description: Top level workflow for the project.  Can be converted to a 
#   cleaner workflow later on.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

############
# Clinical #
############
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_clinical_data.R'))
source(here('analysis', 'script', 'create_drug_dat.R'))
source(here('analysis', 'script', 'save_sunbursts.R'))

fs::dir_create(here("output"))
rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca-clinical.Rmd'),
  output_file = 'genie-bpc-prca-clinical.html',
  output_dir = here('output')
)


###########
# Genomic #
###########
source(here('analysis', 'script', 'input_sanchez.R')) # manual save of pathways.
source(here('analysis', 'script', 'create_ddr_pathway_mapping.R')) # manual save of pathways.
# Need to do once:
source(here('analysis', 'script', 'reshape_cna.R'))
# # run annotate_oncokb.sh from the command line.  See comments on enviro vars.
source(here('analysis', 'script', 'save_oncokb_annotated_data.R'))
# After that the files can be obtained from synid in save_oncokb_annotated_data.R

source(here('analysis', 'script', 'create_gene_panel_dat.R'))
source(here('analysis', 'script', 'process_oncokb_output.R'))
source(here('analysis', 'script', 'add_tmb_to_cpt.R'))
# The above file calls some bioconductor packages that might interfere with 
#  dplyr calls.  Possibly need to reload R after running it.
rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca-genomic.Rmd'),
  output_file = 'genie-bpc-prca-genomic.html',
  output_dir = here('output')
)





##########
# Output #
##########

source(here('analysis', 'script', 'upload_outputs_synapse.R'))
