library(readxl)
library(dplyr)
library(metafor)

# Function to prepare mediators data
prepare_mediators_data <- function(file_path = "data/Dataset_MediatorsNEW.xlsx") {
  
  # Load dataset
  Dataset_MediatorsNEW <- read_excel(file_path)
  DataMediator <- Dataset_MediatorsNEW
  
  # Prepare dataset, define class of variables
  DataMediator$IDES <- as.character(DataMediator$IDES)
  DataMediator$IDd <- as.character(DataMediator$IDd)
  DataMediator$IDp <- as.character(DataMediator$IDp)
  DataMediator$year <- as.numeric(DataMediator$year)
  DataMediator$Pub_Ranking <- as.factor(DataMediator$Pub_Ranking)
  DataMediator$Pub_Status <- as.factor(DataMediator$Pub_Status)
  DataMediator$academic_field_text <- as.character(DataMediator$academic_field_text)
  DataMediator$sample <- as.numeric(DataMediator$sample)
  DataMediator$VRHMD <- as.factor(DataMediator$VRHMD)
  DataMediator$VRPC <- as.factor(DataMediator$VRPC)
  DataMediator$AR <- as.factor(DataMediator$AR)
  DataMediator$ThreeD <- as.factor(DataMediator$ThreeD)
  DataMediator$overlay_element <- as.factor(DataMediator$overlay_element)
  DataMediator$XRfam <- as.factor(DataMediator$XRfam)
  DataMediator$hedonic_utilitarian_product <- as.numeric(DataMediator$hedonic_utilitarian_product)
  DataMediator$fit_uncertainty <- as.numeric(DataMediator$fit_uncertainty)
  DataMediator$brand_familiarity <- as.factor(DataMediator$brand_familiarity)
  DataMediator$uncertainty_avoidance <- as.numeric(DataMediator$uncertainty_avoidance)
  DataMediator$mean_age <- as.numeric(DataMediator$mean_age)
  DataMediator$shopper_behavior <- as.factor(DataMediator$shopper_behavior)
  DataMediator$Design <- as.factor(DataMediator$Design)
  DataMediator$ES_Sample <- as.numeric(DataMediator$ES_Sample)
  DataMediator$ESRAW_Corr <- as.numeric(DataMediator$ESRAW_Corr)
  DataMediator$ESRAW <- as.numeric(DataMediator$ESRAW)
  DataMediator$ESRAW_Z <- as.numeric(DataMediator$ESRAW_Z)
  DataMediator$ES_VAR_Z <- as.numeric(DataMediator$ES_VAR_Z)
  
  return(DataMediator)
}

# Function to perform outlier analysis for mediators data
perform_outlier_analysis_mediators <- function(DataMediator) {
  
  # Cook's distance
  overall <- rma.mv(
    yi = ESRAW_Z,
    V  = ES_VAR_Z,
    random = ~ 1 | IDd/IDES,
    data = DataMediator,
    method = "REML"
  )
  
  cooks_distance <- cooks.distance.rma.mv(
    overall,
    progbar = FALSE,
    reestimate = FALSE
  )
  
  n <- nrow(DataMediator)
  p <- length(coef(overall))
  cutoff <- 4 / (n - p - 1)
  
  outliers_cooks <- data.frame(
    es_id   = DataMediator$IDES[cooks_distance > cutoff],
    study   = DataMediator$IDd[cooks_distance > cutoff],
    cooks_d = cooks_distance[cooks_distance > cutoff]
  ) %>%
    arrange(desc(cooks_d))
  
  dataMeds_wo_outliers <- DataMediator[!(DataMediator$IDES %in% outliers_cooks$es_id),]
  
  list(
    data_wo_outliers = dataMeds_wo_outliers,
    outliers = outliers_cooks,
    cooks_distance = cooks_distance,
    cutoff = cutoff
  )
}

# Initialize mediators data
init_mediators_data <- function(file_path = "data/Dataset_MediatorsNEW.xlsx") {
  DataMediator <- prepare_mediators_data(file_path)
  outlier_result <- perform_outlier_analysis_mediators(DataMediator)
  
  list(
    data_main = DataMediator,
    data_wo_outliers3 = outlier_result$data_wo_outliers,
    outliers = outlier_result$outliers,
    cooks_distance = outlier_result$cooks_distance,
    cutoff = outlier_result$cutoff
  )
}