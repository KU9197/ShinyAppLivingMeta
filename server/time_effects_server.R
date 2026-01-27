
output$time_plot <- renderPlot({
  
  data <- exp_data()$data_wo_outliers3
  
  data$year <- suppressWarnings(as.numeric(as.character(data$year)))
  if ("XRfam" %in% names(data)) {
    data$XRfam <- suppressWarnings(as.numeric(as.character(data$XRfam)))
  }
  
  if (input$time_analysis == "time_overall") {
    
    model_year <- rma.mv(
      ESRAW, ES_VAR,
      random = ~1 | IDd/IDES,
      data = data,
      method = "REML",
      mods = ~ year
    )
    
    years_seq <- sort(unique(data$year))
    pred_list <- predict(model_year, newmods = years_seq)
    
    pred_data <- data.frame(
      year = years_seq,
      pred = pred_list$pred,
      ci.lb = pred_list$ci.lb,
      ci.ub = pred_list$ci.ub
    )
    
    label_data <- pred_data %>%
      filter(year %in% c(min(years_seq, na.rm = TRUE), max(years_seq, na.rm = TRUE))) %>%
      mutate(label = gsub("^0", "", sprintf("%.2f", pred)))
    
    ggplot() +
      geom_jitter(
        data = data,
        aes(x = year, y = ESRAW),
        width = 0.5, alpha = 0.5, color = "grey60"
      ) +
      geom_ribbon(
        data = pred_data,
        aes(x = year, ymin = ci.lb, ymax = ci.ub),
        fill = "grey80", alpha = 0.35
      ) +
      geom_line(
        data = pred_data,
        aes(x = year, y = pred),
        linewidth = 1.2, color = "black"
      ) +
      geom_smooth(
        data = data,
        aes(x = year, y = ESRAW),
        method = "loess", se = FALSE,
        linetype = "dashed", linewidth = 0.8, color = "grey30"
      ) +
      geom_text(
        data = label_data,
        aes(x = year, y = pred, label = label),
        vjust = -1.0, size = 4, fontface = "bold", color = "black"
      ) +
      scale_x_continuous(breaks = seq(min(years_seq, na.rm = TRUE), max(years_seq, na.rm = TRUE), by = 2)) +
      labs(
        title = "Predicted Effect Sizes Over Time",
        x = "Publication Year",
        y = "Cohen's d"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank()
      )
    
  } else if (input$time_analysis == "time_subgroup") {
    
    # Subgroup datasets
    dataVRHMD    <- data[data$VRHMD == 1, ]
    dataVRPC     <- data[data$VRPC == 1, ]
    dataAR       <- data[data$AR == 1, ]
    data3D       <- data[data$ThreeD == 1, ]
    images_data  <- data[data$NonXR_comparison == 0, ]
    video_data   <- data[data$NonXR_comparison == 1, ]
    
    # Models list
    models_list <- list(
      VRHMD  = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataVRHMD,   method = "REML", mods = ~ year),
      VRPC   = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataVRPC,    method = "REML", mods = ~ year),
      AR     = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataAR,      method = "REML", mods = ~ year),
      "3D"   = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = data3D,      method = "REML", mods = ~ year),
      Images = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = images_data, method = "REML", mods = ~ year),
      Videos = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = video_data,  method = "REML", mods = ~ year)
    )
    
    years_seq_list <- list(
      VRHMD  = seq(min(dataVRHMD$year,   na.rm = TRUE), max(dataVRHMD$year,   na.rm = TRUE), by = 2),
      VRPC   = seq(min(dataVRPC$year,    na.rm = TRUE), max(dataVRPC$year,    na.rm = TRUE), by = 2),
      AR     = seq(min(dataAR$year,      na.rm = TRUE), max(dataAR$year,      na.rm = TRUE), by = 2),
      "3D"   = seq(min(data3D$year,      na.rm = TRUE), max(data3D$year,      na.rm = TRUE), by = 2),
      Images = seq(min(images_data$year, na.rm = TRUE), max(images_data$year, na.rm = TRUE), by = 2),
      Videos = seq(min(video_data$year,  na.rm = TRUE), max(video_data$year,  na.rm = TRUE), by = 2)
    )
    
    pred_list <- lapply(names(models_list), function(name) {
      model <- models_list[[name]]
      years_seq <- years_seq_list[[name]]
      
      preds <- sapply(years_seq, function(y) {
        predict(model, newmods = y)$pred
      })
      
      data.frame(year = years_seq, pred = preds, XR_type = name)
    })
    
    pred_df <- bind_rows(pred_list)
    
    ggplot(pred_df, aes(x = year, y = pred, group = XR_type, color = XR_type)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_color_manual(values = c(
        "VRHMD" = "black", "VRPC" = "grey20", "AR" = "grey35",
        "3D" = "grey55", "Images" = "grey65", "Videos" = "grey75"
      )) +
      labs(
        title = "Predicted Effect Sizes Over Time by XR Type",
        x = "Publication Year",
        y = "Cohen's d",
        color = "Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
    
  } else if (input$time_analysis %in% c("fam_overall", "fam_xr", "fam_comp")) {
    
    if (input$time_analysis == "fam_overall") {
      
      model_XRfam <- rma.mv(
        ESRAW, ES_VAR,
        random = ~1 | IDd/IDES,
        data = data,
        method = "REML",
        mods = ~ XRfam
      )
      
      preds <- predict(model_XRfam, newmods = c(0, 1))
      
      pred_df <- data.frame(
        XRfam = factor(c("Not Familiar", "Familiar")),
        pred = preds$pred,
        ci.lb = preds$ci.lb,
        ci.ub = preds$ci.ub,
        XR_type = "Overall"
      )
      
    } else if (input$time_analysis == "fam_xr") {
      
      dataVRHMD <- data[data$VRHMD == 1, ]
      dataVRPC  <- data[data$VRPC == 1, ]
      dataAR    <- data[data$AR == 1, ]
      data3D    <- data[data$ThreeD == 1, ]
      
      models_list <- list(
        Overall = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = data,      method = "REML", mods = ~ XRfam),
        VRHMD   = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataVRHMD, method = "REML", mods = ~ XRfam),
        VRPC    = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataVRPC,  method = "REML", mods = ~ XRfam),
        AR      = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dataAR,    method = "REML", mods = ~ XRfam),
        "3D"    = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = data3D,    method = "REML", mods = ~ XRfam)
      )
      
      pred_list <- lapply(names(models_list), function(name) {
        model <- models_list[[name]]
        preds <- predict(model, newmods = c(0, 1))
        data.frame(
          XRfam = factor(c("Not Familiar", "Familiar")),
          pred = preds$pred,
          ci.lb = preds$ci.lb,
          ci.ub = preds$ci.ub,
          XR_type = name
        )
      })
      
      pred_df <- bind_rows(pred_list)
      
    } else { 
      
      images_data <- data[data$NonXR_comparison == 0, ]
      video_data  <- data[data$NonXR_comparison == 1, ]
      
      models_list <- list(
        Images = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = images_data, method = "REML", mods = ~ XRfam),
        Videos = rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = video_data,  method = "REML", mods = ~ XRfam)
      )
      
      pred_list <- lapply(names(models_list), function(name) {
        model <- models_list[[name]]
        preds <- predict(model, newmods = c(0, 1))
        data.frame(
          XRfam = factor(c("Not Familiar", "Familiar")),
          pred = preds$pred,
          ci.lb = preds$ci.lb,
          ci.ub = preds$ci.ub,
          XR_type = name
        )
      })
      
      pred_df <- bind_rows(pred_list)
    }
    
    fill_colors <- c(
      "Overall" = "grey10", "VRHMD" = "grey30", "VRPC" = "grey45",
      "AR" = "grey60", "3D" = "grey75", "Images" = "grey20", "Videos" = "grey40"
    )
    
    ggplot(pred_df, aes(x = XRfam, y = pred, fill = XR_type)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      geom_errorbar(
        aes(ymin = ci.lb, ymax = ci.ub),
        position = position_dodge(width = 0.8),
        width = 0.15, linewidth = 0.3, color = "grey85"
      ) +
      geom_text(
        aes(label = gsub("^0", "", sprintf("%.3f", pred))),
        position = position_dodge(width = 0.8),
        vjust = 1.5, color = "white", size = 4, fontface = "bold.italic"
      ) +
      geom_text(
        aes(label = XR_type),
        position = position_dodge(width = 0.8),
        vjust = -1.2, size = 4.5, fontface = "bold", color = "black"
      ) +
      scale_fill_manual(values = fill_colors) +
      labs(
        title = "Effect Sizes by XR Familiarity",
        x = "XR Familiarity",
        y = "Cohen's d"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 14)
      )
  }
})


output$time_table <- renderDT({
  
  data <- exp_data()$data_wo_outliers3
  
  data$year <- suppressWarnings(as.numeric(as.character(data$year)))
  if ("XRfam" %in% names(data)) {
    data$XRfam <- suppressWarnings(as.numeric(as.character(data$XRfam)))
  }
  
  # helper: safely extract one coefficient row from metafor model
  extract_term <- function(model, term, label) {
    if (is.null(model)) return(NULL)
    
    b  <- coef(model)
    nm <- names(b)
    if (is.null(nm)) return(NULL)
    
    # metafor commonly uses "intrcpt" for intercept
    if (term == "(Intercept)" && "intrcpt" %in% nm) term <- "intrcpt"
    
    # exact match first
    idx <- which(nm == term)
    
    # fallback: if term is "XRfam" but model has "XRfam1", etc.
    if (length(idx) == 0) idx <- which(startsWith(nm, paste0(term)))
    
    if (length(idx) == 0) return(NULL)
    idx <- idx[1]
    
    data.frame(
      Analysis = label,
      Estimate = as.numeric(b[idx]),
      SE       = as.numeric(model$se[idx]),
      p_value  = as.numeric(model$pval[idx]),
      stringsAsFactors = FALSE
    )
  }
  
  rows <- list()
  
  if (input$time_analysis == "time_overall") {
    
    model_year <- rma.mv(
      ESRAW, ES_VAR,
      random = ~1 | IDd/IDES,
      data = data,
      method = "REML",
      mods = ~ year
    )
    
    rows <- list(
      extract_term(model_year, "(Intercept)", "Intercept"),
      extract_term(model_year, "year", "Year (slope)")
    )
    
  } else if (input$time_analysis == "time_subgroup") {
    
    dataVRHMD   <- data[data$VRHMD == 1, ]
    dataVRPC    <- data[data$VRPC == 1, ]
    dataAR      <- data[data$AR == 1, ]
    data3D      <- data[data$ThreeD == 1, ]
    images_data <- data[data$NonXR_comparison == 0, ]
    video_data  <- data[data$NonXR_comparison == 1, ]
    
    models_data_list <- list(
      VRHMD  = dataVRHMD,
      VRPC   = dataVRPC,
      AR     = dataAR,
      `3D`   = data3D,
      Images = images_data,
      Videos = video_data
    )
    
    for (nm in names(models_data_list)) {
      dsub <- models_data_list[[nm]]
      if (nrow(dsub) < 3) next
      
      m <- tryCatch(
        rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dsub, method = "REML", mods = ~ year),
        error = function(e) NULL
      )
      
      rows <- c(rows, list(
        extract_term(m, "year", paste0(nm, " — Year (slope)"))
      ))
    }
    
  } else if (input$time_analysis %in% c("fam_overall", "fam_xr", "fam_comp")) {
    
    if (input$time_analysis == "fam_overall") {
      
      model_XRfam <- rma.mv(
        ESRAW, ES_VAR,
        random = ~1 | IDd/IDES,
        data = data,
        method = "REML",
        mods = ~ XRfam
      )
      
      rows <- list(
        extract_term(model_XRfam, "(Intercept)", "Intercept"),
        extract_term(model_XRfam, "XRfam", "XR Familiarity (XRfam)")
      )
      
    } else if (input$time_analysis == "fam_xr") {
      
      dataVRHMD <- data[data$VRHMD == 1, ]
      dataVRPC  <- data[data$VRPC == 1, ]
      dataAR    <- data[data$AR == 1, ]
      data3D    <- data[data$ThreeD == 1, ]
      
      models_data_list <- list(
        Overall = data,
        VRHMD   = dataVRHMD,
        VRPC    = dataVRPC,
        AR      = dataAR,
        `3D`    = data3D
      )
      
      for (nm in names(models_data_list)) {
        dsub <- models_data_list[[nm]]
        if (nrow(dsub) < 3) next
        
        m <- tryCatch(
          rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dsub, method = "REML", mods = ~ XRfam),
          error = function(e) NULL
        )
        
        rows <- c(rows, list(
          extract_term(m, "XRfam", paste0(nm, " — XR Familiarity (XRfam)"))
        ))
      }
      
    } else { 
      
      images_data <- data[data$NonXR_comparison == 0, ]
      video_data  <- data[data$NonXR_comparison == 1, ]
      
      models_data_list <- list(
        Images = images_data,
        Videos = video_data
      )
      
      for (nm in names(models_data_list)) {
        dsub <- models_data_list[[nm]]
        if (nrow(dsub) < 3) next
        
        m <- tryCatch(
          rma.mv(ESRAW, ES_VAR, random = ~1 | IDd/IDES, data = dsub, method = "REML", mods = ~ XRfam),
          error = function(e) NULL
        )
        
        rows <- c(rows, list(
          extract_term(m, "XRfam", paste0(nm, " — XR Familiarity (XRfam)"))
        ))
      }
    }
  }
  
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  
  if (is.null(df) || nrow(df) == 0) {
    df <- data.frame(
      Analysis = "No results available for this selection.",
      Estimate = NA_real_,
      SE       = NA_real_,
      p_value  = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  
  datatable(
    df,
    options = list(pageLength = 10, dom = "tip"),
    rownames = FALSE
  )
})