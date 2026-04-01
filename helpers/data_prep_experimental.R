library(readxl)
library(dplyr)
library(metafor)

# Function to prepare experimental data
prepare_experimental_data <- function(file_path = "data/Dataset_MainNEW.xlsx") {
  
  # Load dataset
  Dataset_MainNEW <- read_excel(file_path)
  data_main <- Dataset_MainNEW
  
  # Prepare dataset, define class of variables
  data_main$IDES <- as.character(data_main$IDES)
  data_main$IDd <- as.character(data_main$IDd)
  data_main$IDp <- as.character(data_main$IDp)
  data_main$year <- as.numeric(data_main$year)
  data_main$Pub_Ranking <- as.factor(data_main$Pub_Ranking)
  data_main$Pub_Status <- as.factor(data_main$Pub_Status)
  data_main$academic_field_text <- as.character(data_main$academic_field_text)
  data_main$sample <- as.numeric(data_main$sample)
  data_main$VRHMD <- as.factor(data_main$VRHMD)
  data_main$VRPC <- as.factor(data_main$VRPC)
  data_main$AR <- as.factor(data_main$AR)
  data_main$ThreeD <- as.factor(data_main$ThreeD)
  data_main$overlay_element <- as.factor(data_main$overlay_element)
  data_main$XRfam <- as.factor(data_main$XRfam)
  data_main$Design <- as.factor(data_main$Design)
  data_main$hedonic_utilitarian_product <- as.numeric(data_main$hedonic_utilitarian_product)
  data_main$fit_uncertainty <- as.numeric(data_main$fit_uncertainty)
  data_main$brand_familiarity <- as.factor(data_main$brand_familiarity)
  data_main$uncertainty_avoidance <- as.numeric(data_main$uncertainty_avoidance)
  data_main$mean_age <- as.numeric(data_main$mean_age)
  data_main$shopper_behavior <- as.factor(data_main$shopper_behavior)
  data_main$NonXR_comparison <- as.factor(data_main$NonXR_comparison)
  data_main$outcome_variable <- as.factor(data_main$outcome_variable)
  data_main$outcome_variable_reliability08 <- as.numeric(data_main$outcome_variable_reliability08)
  data_main$ES_Sample <- as.numeric(data_main$ES_Sample)
  data_main$ESRAW <- as.numeric(data_main$ESRAW)
  data_main$ES_VAR <- as.numeric(data_main$ES_VAR)
  
  return(data_main)
}

# Function to perform outlier analysis
perform_outlier_analysis <- function(data_main) {
  
  # Cook's distance
  overall <- rma.mv(
    yi = ESRAW,
    V  = ES_VAR,
    random = ~ 1 | IDd/IDES,
    data = data_main,
    method = "REML"
  )
  
  cooks_distance <- cooks.distance.rma.mv(
    overall,
    progbar = FALSE,
    reestimate = FALSE
  )
  
  n <- nrow(data_main)
  p <- length(coef(overall))
  cutoff <- 4 / (n - p - 1)
  
  outliers_cooks <- data.frame(
    es_id   = data_main$IDES[cooks_distance > cutoff],
    study   = data_main$IDd[cooks_distance > cutoff],
    cooks_d = cooks_distance[cooks_distance > cutoff]
  ) %>%
    arrange(desc(cooks_d))
  
  data_wo_outliers3 <- data_main[!(data_main$IDES %in% outliers_cooks$es_id),]
  
  list(
    data_wo_outliers = data_wo_outliers3,
    outliers = outliers_cooks,
    cooks_distance = cooks_distance,
    cutoff = cutoff
  )
}

# Initialize experimental data
init_experimental_data <- function(file_path = "data/Dataset_MainNEW.xlsx") {
  data_main <- prepare_experimental_data(file_path)
  outlier_result <- perform_outlier_analysis(data_main)
  
  list(
    data_main = data_main,
    data_wo_outliers3 = outlier_result$data_wo_outliers,
    outliers = outlier_result$outliers,
    cooks_distance = outlier_result$cooks_distance,
    cutoff = outlier_result$cutoff
  )
}