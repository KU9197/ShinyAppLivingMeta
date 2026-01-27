
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE) 
source("helpers/data_prep_experimental.R") 
source("helpers/data_prep_correlational.R") 
exp <- init_experimental_data("data/Dataset_MainNEW.xlsx") 
corr <- init_correlational_data("data/Dataset_Correlations.xlsx") 
saveRDS(exp, "data/cache/exp_cache.rds") 
saveRDS(corr, "data/cache/corr_cache.rds")
