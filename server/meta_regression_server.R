# Meta-Regression Server Logic

output$meta_regression_table <- renderDT({
  
  if (input$study_type_meta == "exp") {
    
    data <- exp_data()$data_wo_outliers3
    
    moderators <- c(
      "VRHMD", "AR", "ThreeD", "NonXR_comparison",
      "overlay_element", "XRfam", "product_benefit",
      "fit_uncertainty", "brand_familiarity", "year",
      "uncertainty_avoidance", "mean_age", "shopper_behavior",
      "Pub_Status", "Pub_Ranking", "academic_field",
      "outcome_variable", "Design"
    )
    
    model <- run_meta_regression(data, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate  = round(model$b[, 1], 3),
      SE        = round(model$se, 3),
      z         = round(model$zval, 3),
      p         = format.pval(model$pval, digits = 3),
      CI_Lower  = round(model$ci.lb, 3),
      CI_Upper  = round(model$ci.ub, 3),
      stringsAsFactors = FALSE
    )
    
  } else if (input$study_type_meta == "corr") {
    
    data <- corr_data()$data_wo_outliers3
    
    # original script imputes these in correlational meta-regression
    data$mean_age[is.na(data$mean_age)] <- mean(data$mean_age, na.rm = TRUE)
    data$product_benefit[is.na(data$product_benefit)] <- mean(data$product_benefit, na.rm = TRUE)
    data$fit_uncertainty[is.na(data$fit_uncertainty)] <- mean(data$fit_uncertainty, na.rm = TRUE)
    
    moderators <- c(
      "VRHMD", "AR", "ThreeD", "overlay_element", "XRfam",
      "product_benefit", "fit_uncertainty", "brand_familiarity",
      "year", "uncertainty_avoidance", "mean_age", "shopper_behavior",
      "Pub_Status", "Pub_Ranking", "academic_field", "outcome_variable"
    )
    
    model <- run_meta_regression(data, moderators, yi = "ESRAW_Z", V = "ES_VAR_Z")
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate  = round(model$b[, 1], 3),
      SE        = round(model$se, 3),
      z         = round(model$zval, 3),
      p         = format.pval(model$pval, digits = 3),
      CI_Lower  = round(model$ci.lb, 3),
      CI_Upper  = round(model$ci.ub, 3),
      stringsAsFactors = FALSE
    )
    
  } else {
    results_df <- data.frame(
      Message = "Select a study type to view results.",
      stringsAsFactors = FALSE
    )
  }
  
  datatable(results_df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
})


# Predicted values (EXPERIMENTAL only) — matches original script logic
output$predicted_values_table <- renderDT({
  req(input$study_type_meta == "exp")
  
  data <- exp_data()$data_wo_outliers3
  
  moderators <- c(
    "VRHMD", "AR", "ThreeD", "NonXR_comparison",
    "overlay_element", "XRfam", "product_benefit",
    "fit_uncertainty", "brand_familiarity", "year",
    "uncertainty_avoidance", "mean_age", "shopper_behavior",
    "Pub_Status", "Pub_Ranking", "academic_field",
    "outcome_variable", "Design"
  )
  
  model <- run_meta_regression(data, moderators)
  
  # Prepare a copy for predictions like the original script
  pred_data <- data
  
  # Mean imputation (original at least does mean_age; ok to keep minimal)
  pred_data$mean_age[is.na(pred_data$mean_age)] <- mean(pred_data$mean_age, na.rm = TRUE)
  
  # Binary vars used for scenario predictions (original list)
  binary_vars <- c(
    "VRHMD", "AR", "ThreeD", "NonXR_comparison", "overlay_element", "XRfam",
    "brand_familiarity", "shopper_behavior", "Pub_Status", "Pub_Ranking",
    "academic_field", "outcome_variable", "Design"
  )
  
  # Convert binary vars to numeric 0/1 for mean computation
  for (v in binary_vars) {
    if (v %in% colnames(pred_data)) {
      pred_data[[v]] <- as.numeric(as.character(pred_data[[v]]))
    }
  }
  
  # Create mean-centered vars exactly like original logic
  pred_data$product_benefit_MC       <- scale(pred_data$product_benefit, center = TRUE, scale = FALSE)
  pred_data$fit_uncertainty_MC       <- scale(pred_data$fit_uncertainty, center = TRUE, scale = FALSE)
  pred_data$uncertainty_avoidance_MC <- scale(pred_data$uncertainty_avoidance, center = TRUE, scale = FALSE)
  pred_data$mean_age_MC              <- scale(pred_data$mean_age, center = TRUE, scale = FALSE)
  pred_data$year_MC                  <- scale(pred_data$year, center = TRUE, scale = FALSE)
  
  mc_vars <- c(
    "product_benefit_MC", "fit_uncertainty_MC",
    "year_MC", "uncertainty_avoidance_MC", "mean_age_MC"
  )
  
  # Baseline scenario at means (original newdata1)
  newdata1 <- data.frame(
    VRHMD = mean(pred_data$VRHMD, na.rm = TRUE),
    AR = mean(pred_data$AR, na.rm = TRUE),
    ThreeD = mean(pred_data$ThreeD, na.rm = TRUE),
    NonXR_comparison = mean(pred_data$NonXR_comparison, na.rm = TRUE),
    overlay_element = mean(pred_data$overlay_element, na.rm = TRUE),
    XRfam = mean(pred_data$XRfam, na.rm = TRUE),
    product_benefit_MC = mean(pred_data$product_benefit_MC, na.rm = TRUE),
    fit_uncertainty_MC = mean(pred_data$fit_uncertainty_MC, na.rm = TRUE),
    brand_familiarity = mean(pred_data$brand_familiarity, na.rm = TRUE),
    year_MC = mean(pred_data$year_MC, na.rm = TRUE),
    uncertainty_avoidance_MC = mean(pred_data$uncertainty_avoidance_MC, na.rm = TRUE),
    mean_age_MC = mean(pred_data$mean_age_MC, na.rm = TRUE),
    shopper_behavior = mean(pred_data$shopper_behavior, na.rm = TRUE),
    Pub_Status = mean(pred_data$Pub_Status, na.rm = TRUE),
    Pub_Ranking = mean(pred_data$Pub_Ranking, na.rm = TRUE),
    academic_field = mean(pred_data$academic_field, na.rm = TRUE),
    outcome_variable = mean(pred_data$outcome_variable, na.rm = TRUE),
    Design = mean(pred_data$Design, na.rm = TRUE)
  )
  
  predictions <- list()
  
  # Binary scenarios (0/1)
  for (var in binary_vars) {
    for (val in c(0, 1)) {
      scenario <- newdata1
      scenario[[var]] <- val
      pr <- predict(model, newmods = as.matrix(scenario))
      predictions[[paste0(var, " = ", val)]] <- pr
    }
  }
  
  # Continuous scenarios (mean ± 1.5 SD)
  for (var in mc_vars) {
    mean_val <- mean(pred_data[[var]], na.rm = TRUE)
    sd_val <- sd(pred_data[[var]], na.rm = TRUE)
    
    for (val in c(mean_val - 1.5 * sd_val, mean_val + 1.5 * sd_val)) {
      scenario <- newdata1
      scenario[[var]] <- val
      pr <- predict(model, newmods = as.matrix(scenario))
      predictions[[paste0(var, " = ", round(val, 2))]] <- pr
    }
  }
  
  df <- do.call(rbind, lapply(names(predictions), function(label) {
    pr <- predictions[[label]]
    data.frame(
      Scenario = label,
      Predicted = round(pr$pred, 3),
      CI_Lower  = round(pr$ci.lb, 3),
      CI_Upper  = round(pr$ci.ub, 3),
      stringsAsFactors = FALSE
    )
  }))
  
  datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
})


# Descriptives (experimental only) — now binary + continuous tables
output$descriptives_binary_table <- renderDT({
  req(input$study_type_meta == "desc_exp")
  
  data <- exp_data()$data_wo_outliers3
  N_sample <- data[!duplicated(data$IDd), ]
  
  binary_vars <- c(
    "AR", "ThreeD", "VRHMD", "VRPC", "overlay_element",
    "brand_familiarity", "shopper_behavior", "XRfam",
    "Pub_Status", "Pub_Ranking", "academic_field",
    "NonXR_comparison", "Design", "outcome_variable"
  )
  
  df <- do.call(rbind, lapply(binary_vars, function(var) {
    if (var %in% colnames(data)) {
      data.frame(
        Variable = var,
        Yes_n = sum(as.character(data[[var]]) == "1", na.rm = TRUE),
        No_n = sum(as.character(data[[var]]) == "0", na.rm = TRUE),
        Studies_yes = dplyr::n_distinct(data$IDd[as.character(data[[var]]) == "1"]),
        Studies_no  = dplyr::n_distinct(data$IDd[as.character(data[[var]]) == "0"]),
        Effects_N = sum(!is.na(data[[var]])),
        Samples_N = sum(!is.na(N_sample[[var]])),
        stringsAsFactors = FALSE
      )
    }
  }))
  
  datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
})

output$descriptives_continuous_table <- renderDT({
  req(input$study_type_meta == "desc_exp")
  
  data <- exp_data()$data_wo_outliers3
  N_sample <- data[!duplicated(data$IDd), ]
  
  cont_vars <- c("product_benefit", "fit_uncertainty", "uncertainty_avoidance", "mean_age", "year")
  
  df <- do.call(rbind, lapply(cont_vars, function(var) {
    if (var %in% colnames(data)) {
      data.frame(
        Variable = var,
        Mean = round(mean(data[[var]], na.rm = TRUE), 2),
        SD   = round(sd(data[[var]], na.rm = TRUE), 2),
        Min  = round(min(data[[var]], na.rm = TRUE), 2),
        Max  = round(max(data[[var]], na.rm = TRUE), 2),
        Effects_N = sum(!is.na(data[[var]])),
        Samples_N = sum(!is.na(N_sample[[var]])),
        stringsAsFactors = FALSE
      )
    }
  }))
  
  datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
})
