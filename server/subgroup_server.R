# Subgroup Analysis Server Logic

# Helper to extract a clean coefficient table
extract_coef_table <- function(model) {
  data.frame(
    Predictor = rownames(model$b),
    Estimate  = round(model$b[, 1], 3),
    SE        = round(model$se, 3),
    z         = round(model$zval, 3),
    p         = format.pval(model$pval, digits = 3),
    CI_Lower  = round(model$ci.lb, 3),
    CI_Upper  = round(model$ci.ub, 3),
    stringsAsFactors = FALSE
  )
}

output$subgroup_att_table <- renderDT({
  req(input$subgroup_type == "outcome")
  
  data <- exp_data()$data_wo_outliers3
  attitudes_data <- data[as.character(data$outcome_variable) == "1", ]
  req(nrow(attitudes_data) > 0)
  
  moderators_att <- c(
    "VRHMD", "AR", "ThreeD", "NonXR_comparison",
    "overlay_element", "XRfam", "product_benefit",
    "fit_uncertainty", "brand_familiarity", "year",
    "uncertainty_avoidance", "mean_age", "shopper_behavior",
    "Pub_Status", "Pub_Ranking", "academic_field", "Design"
  )
  
  model_att <- run_meta_regression(attitudes_data, moderators_att)
  
  datatable(extract_coef_table(model_att),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_beh_table <- renderDT({
  req(input$subgroup_type == "outcome")
  
  data <- exp_data()$data_wo_outliers3
  beh_data <- data[as.character(data$outcome_variable) == "0", ]
  req(nrow(beh_data) > 0)
  
  moderators_att <- c(
    "VRHMD", "AR", "ThreeD", "NonXR_comparison",
    "overlay_element", "XRfam", "product_benefit",
    "fit_uncertainty", "brand_familiarity", "year",
    "uncertainty_avoidance", "mean_age", "shopper_behavior",
    "Pub_Status", "Pub_Ranking", "academic_field", "Design"
  )
  
  model_beh <- run_meta_regression(beh_data, moderators_att)
  
  datatable(extract_coef_table(model_beh),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_img_table <- renderDT({
  req(input$subgroup_type == "comparison")
  
  data <- exp_data()$data_wo_outliers3
  images_data <- data[as.character(data$NonXR_comparison) == "0", ]
  req(nrow(images_data) > 0)
  
  moderators_img <- c(
    "VRHMD", "AR", "ThreeD",
    "overlay_element", "XRfam", "product_benefit",
    "fit_uncertainty", "brand_familiarity", "year",
    "uncertainty_avoidance", "mean_age", "shopper_behavior",
    "Pub_Status", "Pub_Ranking", "academic_field",
    "outcome_variable", "Design"
  )
  
  model_img <- run_meta_regression(images_data, moderators_img)
  
  datatable(extract_coef_table(model_img),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_vid_table <- renderDT({
  req(input$subgroup_type == "comparison")
  
  data <- exp_data()$data_wo_outliers3
  video_data <- data[as.character(data$NonXR_comparison) == "1", ]
  req(nrow(video_data) > 0)
  
  moderators_img <- c(
    "VRHMD", "AR", "ThreeD",
    "overlay_element", "XRfam", "product_benefit",
    "fit_uncertainty", "brand_familiarity", "year",
    "uncertainty_avoidance", "mean_age", "shopper_behavior",
    "Pub_Status", "Pub_Ranking", "academic_field",
    "outcome_variable", "Design"
  )
  
  model_vid <- run_meta_regression(video_data, moderators_img)
  
  datatable(extract_coef_table(model_vid),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_ar_table <- renderDT({
  req(input$subgroup_type == "xr_type")
  
  data <- exp_data()$data_wo_outliers3
  dataAR <- data[as.character(data$AR) == "1", ]
  req(nrow(dataAR) > 0)
  
  moderators_xr <- c(
    "NonXR_comparison", "overlay_element", "XRfam",
    "product_benefit", "fit_uncertainty", "brand_familiarity",
    "year", "uncertainty_avoidance", "mean_age",
    "shopper_behavior", "Pub_Status", "Pub_Ranking",
    "academic_field", "outcome_variable", "Design"
  )
  
  model_ar <- run_meta_regression(dataAR, moderators_xr)
  
  datatable(extract_coef_table(model_ar),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_vrhmd_table <- renderDT({
  req(input$subgroup_type == "xr_type")
  
  data <- exp_data()$data_wo_outliers3
  dataVRHMD <- data[as.character(data$VRHMD) == "1", ]
  req(nrow(dataVRHMD) > 0)
  
  moderators_xr <- c(
    "NonXR_comparison", "overlay_element", "XRfam",
    "product_benefit", "fit_uncertainty", "brand_familiarity",
    "year", "uncertainty_avoidance", "mean_age",
    "shopper_behavior", "Pub_Status", "Pub_Ranking",
    "academic_field", "outcome_variable", "Design"
  )
  
  model_vrhmd <- run_meta_regression(dataVRHMD, moderators_xr)
  
  datatable(extract_coef_table(model_vrhmd),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_vrpc_table <- renderDT({
  req(input$subgroup_type == "xr_type")
  
  data <- exp_data()$data_wo_outliers3
  dataVRPC <- data[as.character(data$VRPC) == "1", ]
  req(nrow(dataVRPC) > 0)
  
  moderators_xr <- c(
    "NonXR_comparison", "overlay_element", "XRfam",
    "product_benefit", "fit_uncertainty", "brand_familiarity",
    "year", "uncertainty_avoidance", "mean_age",
    "shopper_behavior", "Pub_Status", "Pub_Ranking",
    "academic_field", "outcome_variable", "Design"
  )
  
  model_vrpc <- run_meta_regression(dataVRPC, moderators_xr)
  
  datatable(extract_coef_table(model_vrpc),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})

output$subgroup_3d_table <- renderDT({
  req(input$subgroup_type == "xr_type")
  
  data <- exp_data()$data_wo_outliers3
  data3D <- data[as.character(data$ThreeD) == "1", ]
  req(nrow(data3D) > 0)
  
  moderators_xr <- c(
    "NonXR_comparison", "overlay_element", "XRfam",
    "product_benefit", "fit_uncertainty", "brand_familiarity",
    "year", "uncertainty_avoidance", "mean_age",
    "shopper_behavior", "Pub_Status", "Pub_Ranking",
    "academic_field", "outcome_variable", "Design"
  )
  
  model_3d <- run_meta_regression(data3D, moderators_xr)
  
  datatable(extract_coef_table(model_3d),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE)
})
