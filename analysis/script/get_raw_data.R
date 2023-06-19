# Description: Grabs the raw data from Synapse and stores it in the data-raw folder.
#  We are not using the genieBPC function because it does not appear to support
#  personal access tokens at the time of this writing, and the filtering on 
#  some of the data may or may not be useful.
# Below we pull the data that way for sunburst plots only.
# Author: Alex Paynter

# The Synapse folder containing the clinical data files.
synid_clin_data <- "syn50612196"

library(cli)
library(synapser)
library(purrr)
library(dplyr)
library(here)
library(stringr)
library(magrittr)

synLogin()

# create directories for data and data-raw
dir.create(here("data"), showWarnings = F)
dir.create(here("data-raw"), showWarnings = F)

df_clin_children <- synGetChildren(synid_clin_data) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble)

if (any(stringr::str_detect(df_clin_children$name, ".csv^"))) {
  warning("Non-CSV files unexpectedly contained in {synid_clin_data}.")
}

syn_store_in_dataraw <- function(sid) {
  synGet(entity = sid, downloadLocation = here("data-raw"))
}

purrr::walk(.x = df_clin_children$id, 
            .f = syn_store_in_dataraw)




# genieBPC data pulls
# In order to create the sunburst plots, we'll leverage the genieBPC to pull the
#   data in the format they require.
# To run this, you will need to set up a session with synapse using 
#   genieBPC::set_synapse_credentials() outside of this script (can't be tracked).
#   This requires username and password access at the moment.
library(genieBPC)
data_list <- pull_data_synapse("Prostate", version = "v1.2-consortium")
dir.create(here("data-raw", "genieBPC-style"))
saveRDS(object = data_list, 
        file = here("data-raw", "genieBPC-style", "data_list.rds"))