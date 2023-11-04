#BiocManager::install('maftools')

library(maftools)

# load my other stuff for convenience:
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

?maftools::somaticInteractions

# example they give:
laml.maf <- system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools')
laml <- read.maf(maf = laml.maf)
somaticInteractions(
  maf = laml,
  top = 5
)

laml %>% lobstr::tree(., max_depth = 2)
laml %>% glimpse

# FDR (assume BF).
si_rtn <- somaticInteractions(
  maf = laml,
  top = 10,
  plotPadj = T
) 
lobstr::tree(si_rtn) # so it returns the dataframe here.  The plot is spit out as it returns.  Grosssssss, but useful for me to compare P values with.

# Cherry picking some genes that have significant P values here:
si_rtn %>% 
  filter(
    gene1 %in% c("DNMT3A") & gene2 %in% c("NPM1", "IDH1")
  )
# Can we reproduce this P value?
fisher.test(x = matrix(c(128, 32, 17, 16), nrow = 2, byrow = T))
# Yep.

si_rtn %>% 
  filter(gene1 %in% "FLT3" & gene2 %in% "TP53")



# Observations: Only 1 test is done.  They just do (literally)
# ifelse(f$estimate > 1,-log10(f$p.val), log10(f$p.val))

somaticInteractions(
  maf = laml,
  top = 5,
  plotPadj = T,
  showCounts = T, # show counts and ignore P values.
  countType = "all", # shows co-occur / mut excl.
  pvalue = -Inf,
  colPal = "PuOr", # just for fun.
) 

somaticInteractions(
  maf = laml,
  top = 10,
  plotPadj = T
)

  
