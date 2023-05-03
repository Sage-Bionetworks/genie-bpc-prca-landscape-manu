output_prca_synid <- "syn51393619" #2023-04-17-BrCa-landscape-paper-outputs

library(synapser)

synLogin()
synapser::File(here("analysis", "reports", "main.html"),
               parent = output_prca_synid) %>%
  synStore()
