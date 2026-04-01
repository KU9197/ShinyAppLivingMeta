# Robustness Checks Server Logic

output$robustness_table <- renderDT({
  
  data_exp <- exp_data()
  data_wo_outliers3 <- data_exp$data_wo_outliers3
  data_main <- data_exp$data_main
  
  if(input$robustness_type == "univariate") {
    
    # Run univariate meta-regressions
    moderators <- c("AR", "VRHMD", "VRPC", "ThreeD", "overlay_element", 
                    "hedonic_utilitarian_product", "fit_uncertainty", "brand_familiarity", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior", 
                    "Pub_Status", "Pub_Ranking", "academic_field", 
                    "NonXR_comparison", "outcome_variable", "Design", "year", "XRfam")
    
    # Mean center continuous variables
    continuous_vars <- c("hedonic_utilitarian_product", "fit_uncertainty", 
                         "uncertainty_avoidance", "mean_age", "year")
    
    for(var in continuous_vars) {
      mc_name <- paste0(var, "_MC")
      data_wo_outliers3[[mc_name]] <- scale(data_wo_outliers3[[var]], 
                                            center = TRUE, scale = FALSE)
    }
    
    # Update moderator names
    moderators <- gsub("hedonic_utilitarian_product", "hedonic_utilitarian_product_MC", moderators)
    moderators <- gsub("fit_uncertainty", "fit_uncertainty_MC", moderators)
    moderators <- gsub("uncertainty_avoidance", "uncertainty_avoidance_MC", moderators)
    moderators <- gsub("mean_age", "mean_age_MC", moderators)
    moderators <- gsub("year", "year_MC", moderators)
    
    results_list <- lapply(moderators, function(mod_var) {
      formula <- as.formula(paste("ESRAW ~", mod_var))
      mod <- rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, 
                    data = data_wo_outliers3, method = "REML", mods = formula)
      res <- summary(mod)
      
      if(nrow(res$beta) > 1) {
        tibble(
          Predictor = mod_var,
          Estimate = round(res$beta[2,1], 3),
          SE = round(res$se[2], 3),
          z = round(res$zval[2], 3),
          p = format.pval(res$pval[2], digits = 3)
        )
      }
    })
    
    results_df <- bind_rows(results_list)
    
  } else if(input$robustness_type == "with_outliers") {
    
    data_main$mean_age[is.na(data_main$mean_age)] <- mean(data_main$mean_age, na.rm = TRUE)
    
    moderators <- c("VRHMD", "AR", "ThreeD", "NonXR_comparison", 
                    "overlay_element", "XRfam", "hedonic_utilitarian_product", 
                    "fit_uncertainty", "brand_familiarity", "year", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior", 
                    "Pub_Status", "Pub_Ranking", "academic_field", 
                    "outcome_variable", "Design")
    
    model <- run_meta_regression(data_main, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate = round(model$b[,1], 3),
      SE = round(model$se, 3),
      z = round(model$zval, 3),
      p = format.pval(model$pval, digits = 3),
      stringsAsFactors = FALSE
    )
    
  } else if(input$robustness_type == "3sd_outliers") {
    
    # 3SD outlier detection
    mean_es <- mean(data_main$ESRAW)
    sd_es <- sd(data_main$ESRAW)
    cutoff <- 3 * sd_es
    outliers <- abs(data_main$ESRAW - mean_es) > cutoff
    data_wo_outliers3SD <- data_main[!outliers, ]
    
    data_wo_outliers3SD$mean_age[is.na(data_wo_outliers3SD$mean_age)] <- 
      mean(data_wo_outliers3SD$mean_age, na.rm = TRUE)
    
    moderators <- c("VRHMD", "AR", "ThreeD", "NonXR_comparison", 
                    "overlay_element", "XRfam", "hedonic_utilitarian_product", 
                    "fit_uncertainty", "brand_familiarity", "year", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior", 
                    "Pub_Status", "Pub_Ranking", "academic_field", 
                    "outcome_variable", "Design")
    
    model <- run_meta_regression(data_wo_outliers3SD, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate = round(model$b[,1], 3),
      SE = round(model$se, 3),
      z = round(model$zval, 3),
      p = format.pval(model$pval, digits = 3),
      stringsAsFactors = FALSE
    )
    
  } else if(input$robustness_type == "theoretical") {
    
    data_wo_outliers3$mean_age[is.na(data_wo_outliers3$mean_age)] <- 
      mean(data_wo_outliers3$mean_age, na.rm = TRUE)
    
    moderators <- c("VRHMD", "AR", "ThreeD", "NonXR_comparison", 
                    "overlay_element", "XRfam", "hedonic_utilitarian_product", 
                    "fit_uncertainty", "brand_familiarity", "year", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior")
    
    model <- run_meta_regression(data_wo_outliers3, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate = round(model$b[,1], 3),
      SE = round(model$se, 3),
      z = round(model$zval, 3),
      p = format.pval(model$pval, digits = 3),
      stringsAsFactors = FALSE
    )
    
  } else if(input$robustness_type == "controls") {
    
    data_wo_outliers3$mean_age[is.na(data_wo_outliers3$mean_age)] <- 
      mean(data_wo_outliers3$mean_age, na.rm = TRUE)
    
    moderators <- c("VRHMD", "AR", "ThreeD", "NonXR_comparison", 
                    "overlay_element", "XRfam", "hedonic_utilitarian_product", 
                    "fit_uncertainty", "brand_familiarity", "year", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior", 
                    "Pub_Status", "Pub_Ranking", "academic_field", 
                    "outcome_variable", "Design", "outcome_variable_reliability08", 
                    "power_distance", "individualism", "masculinity", 
                    "long_term_orientation", "indulgence")
    
    model <- run_meta_regression(data_wo_outliers3, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate = round(model$b[,1], 3),
      SE = round(model$se, 3),
      z = round(model$zval, 3),
      p = format.pval(model$pval, digits = 3),
      stringsAsFactors = FALSE
    )
    
  } else if(input$robustness_type == "median_age") {
    
    mean_value <- mean(data_wo_outliers3$mean_age, na.rm = TRUE)
    median_value <- median(data_wo_outliers3$mean_age, na.rm = TRUE)
    data_wo_outliers3$mean_age[data_wo_outliers3$mean_age == mean_value] <- median_value
    
    moderators <- c("VRHMD", "AR", "ThreeD", "NonXR_comparison", 
                    "overlay_element", "XRfam", "hedonic_utilitarian_product", 
                    "fit_uncertainty", "brand_familiarity", "year", 
                    "uncertainty_avoidance", "mean_age", "shopper_behavior", 
                    "Pub_Status", "Pub_Ranking", "academic_field", 
                    "outcome_variable", "Design")
    
    model <- run_meta_regression(data_wo_outliers3, moderators)
    
    results_df <- data.frame(
      Predictor = rownames(model$b),
      Estimate = round(model$b[,1], 3),
      SE = round(model$se, 3),
      z = round(model$zval, 3),
      p = format.pval(model$pval, digits = 3),
      stringsAsFactors = FALSE
    )
    
  } else {
    # Placeholder for other robustness checks
    results_df <- data.frame(
      Predictor = character(),
      Estimate = numeric(),
      SE = numeric(),
      z = numeric(),
      p = character(),
      stringsAsFactors = FALSE
    )
  }
  
  datatable(results_df, options = list(pageLength = 20), rownames = FALSE)
})