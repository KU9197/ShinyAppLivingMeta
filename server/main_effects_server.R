# Main Effects Server Logic

# store the last displayed main-effects table for pairwise comparisons
main_effects_df <- reactiveVal(NULL)

# Load data reactively
# exp_data <- reactive({
#  init_experimental_data()
#})

# corr_data <- reactive({
#  init_correlational_data()
#})

# Get current dataset
current_data_main <- reactive({
  if (input$filter_type == "correlational") {
    corr_data()$data_wo_outliers3
  } else {
    exp_data()$data_wo_outliers3
  }
})

# Helper: build full interaction config (matches UI choices)
build_interaction_config <- function() {
  cfg <- list()
  
  # all XRs combined vs comparison + outcome
  cfg[["xr_img_att"]] <- list(
    filter = 'NonXR_comparison == "0" & outcome_variable == "1"',
    label  = "XRs vs. Images - Attitudinal"
  )
  cfg[["xr_img_beh"]] <- list(
    filter = 'NonXR_comparison == "0" & outcome_variable == "0"',
    label  = "XRs vs. Images - Behavioral"
  )
  cfg[["xr_vid_att"]] <- list(
    filter = 'NonXR_comparison == "1" & outcome_variable == "1"',
    label  = "XRs vs. Videos - Attitudinal"
  )
  cfg[["xr_vid_beh"]] <- list(
    filter = 'NonXR_comparison == "1" & outcome_variable == "0"',
    label  = "XRs vs. Videos - Behavioral"
  )
  
  # AR block
  cfg[["ar_nonxr_att"]] <- list(
    filter = 'AR == "1" & outcome_variable == "1"',
    label  = "AR vs. Non-XRs - Attitudinal"
  )
  cfg[["ar_nonxr_beh"]] <- list(
    filter = 'AR == "1" & outcome_variable == "0"',
    label  = "AR vs. Non-XRs - Behavioral"
  )
  cfg[["ar_img_overall"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "0"',
    label  = "AR vs. Images - Overall"
  )
  cfg[["ar_img_att"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "0" & outcome_variable == "1"',
    label  = "AR vs. Images - Attitudinal"
  )
  cfg[["ar_img_beh"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "0" & outcome_variable == "0"',
    label  = "AR vs. Images - Behavioral"
  )
  cfg[["ar_vid_overall"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "1"',
    label  = "AR vs. Videos - Overall"
  )
  cfg[["ar_vid_att"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "1" & outcome_variable == "1"',
    label  = "AR vs. Videos - Attitudinal"
  )
  cfg[["ar_vid_beh"]] <- list(
    filter = 'AR == "1" & NonXR_comparison == "1" & outcome_variable == "0"',
    label  = "AR vs. Videos - Behavioral"
  )
  
  # VRHMD, VRPC, 3D blocks
  techs <- list(
    vrhmd = "VRHMD",
    vrpc  = "VRPC",
    `3d`  = "ThreeD"
  )
  
  for (short in names(techs)) {
    tech <- techs[[short]]
    
    cfg[[paste0(short, "_nonxr_att")]] <- list(
      filter = paste0(tech, ' == "1" & outcome_variable == "1"'),
      label  = paste0(toupper(short), " vs. Non-XRs - Attitudinal")
    )
    cfg[[paste0(short, "_nonxr_beh")]] <- list(
      filter = paste0(tech, ' == "1" & outcome_variable == "0"'),
      label  = paste0(toupper(short), " vs. Non-XRs - Behavioral")
    )
    
    cfg[[paste0(short, "_img_overall")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "0"'),
      label  = paste0(toupper(short), " vs. Images - Overall")
    )
    cfg[[paste0(short, "_img_att")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "0" & outcome_variable == "1"'),
      label  = paste0(toupper(short), " vs. Images - Attitudinal")
    )
    cfg[[paste0(short, "_img_beh")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "0" & outcome_variable == "0"'),
      label  = paste0(toupper(short), " vs. Images - Behavioral")
    )
    
    cfg[[paste0(short, "_vid_overall")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "1"'),
      label  = paste0(toupper(short), " vs. Videos - Overall")
    )
    cfg[[paste0(short, "_vid_att")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "1" & outcome_variable == "1"'),
      label  = paste0(toupper(short), " vs. Videos - Attitudinal")
    )
    cfg[[paste0(short, "_vid_beh")]] <- list(
      filter = paste0(tech, ' == "1" & NonXR_comparison == "1" & outcome_variable == "0"'),
      label  = paste0(toupper(short), " vs. Videos - Behavioral")
    )
  }
  
  cfg
}

# Main effects table
output$main_effects_table <- renderDT({
  
  data <- current_data_main()
  df <- NULL
  
  if (input$filter_type != "correlational") {
    # Experimental studies
    
    if (input$filter_type == "overall") {
      result <- run_meta_analysis(data)
      df <- format_results(result, "Overall Effect")
      
    } else if (input$filter_type == "xr_type") {
      results_list <- lapply(input$xr_types, function(xr) {
        subset_data <- data[data[[xr]] == "1", ]
        result <- run_meta_analysis(subset_data)
        format_results(result, xr)
      })
      df <- do.call(rbind, results_list)
      
    } else if (input$filter_type == "comparison") {
      results_list <- list()
      
      if ("overall_img" %in% input$comparison_types) {
        subset_data <- data[data$NonXR_comparison == "0", ]
        results_list[["Overall Images"]] <- format_results(
          run_meta_analysis(subset_data), "Overall Images"
        )
      }
      if ("img_only" %in% input$comparison_types) {
        subset_data <- data[data$image == "1", ]
        results_list[["Images Only"]] <- format_results(
          run_meta_analysis(subset_data), "Images Only"
        )
      }
      if ("img_text" %in% input$comparison_types) {
        subset_data <- data[data$imtext == "1", ]
        results_list[["Images + Text"]] <- format_results(
          run_meta_analysis(subset_data), "Images + Text"
        )
      }
      if ("overall_vid" %in% input$comparison_types) {
        subset_data <- data[data$NonXR_comparison == "1", ]
        results_list[["Overall Videos"]] <- format_results(
          run_meta_analysis(subset_data), "Overall Videos"
        )
      }
      if ("vid_only" %in% input$comparison_types) {
        subset_data <- data[data$video == "1", ]
        results_list[["Videos Only"]] <- format_results(
          run_meta_analysis(subset_data), "Videos Only"
        )
      }
      if ("vid_text" %in% input$comparison_types) {
        subset_data <- data[data$vidtext == "1", ]
        results_list[["Videos + Text"]] <- format_results(
          run_meta_analysis(subset_data), "Videos + Text"
        )
      }
      
      df <- do.call(rbind, results_list)
      
    } else if (input$filter_type == "outcome") {
      results_list <- list()
      
      if ("attitude" %in% input$outcome_types) {
        subset_data <- data[data$outcome_variable == "1", ]
        results_list[["Attitudinal"]] <- format_results(
          run_meta_analysis(subset_data), "Attitudinal Responses"
        )
      }
      if ("intention" %in% input$outcome_types) {
        subset_data <- data[data$intention == "1", ]
        results_list[["Intentions"]] <- format_results(
          run_meta_analysis(subset_data), "Behavioral Intentions"
        )
      }
      if ("behavior" %in% input$outcome_types) {
        subset_data <- data[data$behavior == "1", ]
        results_list[["Behavior"]] <- format_results(
          run_meta_analysis(subset_data), "Behavior"
        )
      }
      if ("behavioral" %in% input$outcome_types) {
        subset_data <- data[data$outcome_variable == "0", ]
        results_list[["Behavioral"]] <- format_results(
          run_meta_analysis(subset_data), "Behavioral Responses"
        )
      }
      
      df <- do.call(rbind, results_list)
      
    } else if (input$filter_type == "interaction") {
      analysis_config <- build_interaction_config()
      config <- analysis_config[[input$interaction_type]]
      req(!is.null(config))
      
      subset_data <- data %>% filter(eval(parse(text = config$filter)))
      req(nrow(subset_data) > 0)
      
      result <- run_meta_analysis(subset_data)
      df <- format_results(result, config$label)
    }
    
  } else {
    # Correlational studies
    yi <- "ESRAW_Z"
    V  <- "ES_VAR_Z"
    
    # Helper: overwrite displayed Estimate/CI/SE with transformed r scale (ztor),
    # matching original script intent: transf.ztor(model$beta)
    apply_ztor_to_table <- function(df, result) {
      z_est <- as.numeric(result$estimate)
      z_lb  <- as.numeric(result$ci_lb)
      z_ub  <- as.numeric(result$ci_ub)
      
      r_est <- as.numeric(transf.ztor(z_est))
      r_lb  <- as.numeric(transf.ztor(z_lb))
      r_ub  <- as.numeric(transf.ztor(z_ub))
      
      df$Estimate  <- round(r_est, 3)
      df$CI_Lower  <- round(r_lb, 3)
      df$CI_Upper  <- round(r_ub, 3)
      df$SE        <- round((r_ub - r_lb) / (2 * 1.96), 3)
      
      df
    }
    
    if (input$corr_filter == "overall") {
      result <- run_meta_analysis(data, yi = yi, V = V)
      df <- format_results(result, "Overall Effect")
      df <- apply_ztor_to_table(df, result)
      
    } else if (input$corr_filter == "attitude") {
      subset_data <- data[data$outcome_variable == "1", ]
      result <- run_meta_analysis(subset_data, yi = yi, V = V)
      df <- format_results(result, "Attitudinal Responses")
      df <- apply_ztor_to_table(df, result)
      
    } else if (input$corr_filter == "behavioral") {
      subset_data <- data[data$outcome_variable == "0", ]
      result <- run_meta_analysis(subset_data, yi = yi, V = V)
      df <- format_results(result, "Behavioral Responses")
      df <- apply_ztor_to_table(df, result)
      
    } else {
      subset_data <- data[data[[input$corr_filter]] == "1", ]
      result <- run_meta_analysis(subset_data, yi = yi, V = V)
      df <- format_results(result, input$corr_filter)
      df <- apply_ztor_to_table(df, result)
    }
  }
  
  # store df for pairwise table when relevant
  if (input$filter_type %in% c("xr_type", "comparison", "outcome")) {
    main_effects_df(df)
  } else {
    main_effects_df(NULL)
  }
  
  datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
})

# Forest plot
output$forest_plot <- renderPlot({
  req(input$filter_type == "overall")
  
  data <- current_data_main()
  
  # Create forest plot
  data$sei <- sqrt(data$ES_VAR)
  data$ci_low  <- data$ESRAW - 1.96 * data$sei
  data$ci_high <- data$ESRAW + 1.96 * data$sei
  data <- data[order(data$ESRAW), ]
  data$y <- nrow(data):1
  
  model1 <- rma.mv(
    ESRAW, ES_VAR,
    random = ~1 | IDd/IDES,
    data = data,
    method = "REML"
  )
  
  overall <- data.frame(
    estimate = as.numeric(coef(model1)),
    ci_low   = model1$ci.lb,
    ci_high  = model1$ci.ub,
    ypos     = 0
  )
  
  ggplot(data, aes(x = ESRAW, y = y)) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                   height = 0.2, color = "grey30") +
    geom_point(shape = 21, color = "black", fill = "white",
               size = 2, stroke = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_errorbarh(data = overall,
                   aes(xmin = ci_low, xmax = ci_high, y = ypos),
                   inherit.aes = FALSE, height = 0.4,
                   linewidth = 1.2, color = "black") +
    geom_point(data = overall, aes(x = estimate, y = ypos),
               inherit.aes = FALSE, shape = 18, size = 3, color = "black") +
    annotate(
      "text",
      x = 0, y = -0.8,
      label = sprintf(
        "Average effect size (Cohen's d) = %.2f\n[%.2f, %.2f]",
        overall$estimate, overall$ci_low, overall$ci_high
      ),
      size = 5,
      hjust = 0.5
    ) +
    labs(x = "Effect size (Cohen's d) with 95% CI", y = NULL) +
    scale_y_reverse() +
    theme_minimal(base_size = 16) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
})

# Dataset description
output$dataset_description <- renderDT({
  data <- current_data_main()
  desc_table <- generate_dataset_description(data)
  datatable(desc_table, options = list(pageLength = 15, dom = "t"), rownames = FALSE)
})

# Pairwise comparisons (original MainEffects logic via CI -> SE)
output$pairwise_table <- renderDT({
  req(input$filter_type %in% c("xr_type", "comparison", "outcome"))
  
  df <- main_effects_df()
  req(!is.null(df))
  req(all(c("Analysis", "Estimate", "CI_Lower", "CI_Upper") %in% names(df)))
  
  pw <- make_pairwise_table_original(df)
  
  datatable(pw, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
})