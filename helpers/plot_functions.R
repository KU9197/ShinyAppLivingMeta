library(ggplot2)

# Function to create forest plot
create_forest_plot <- function(data, model) {
  
  data$sei <- sqrt(data$ES_VAR)
  data$ci_low  <- data$ESRAW - 1.96 * data$sei
  data$ci_high <- data$ESRAW + 1.96 * data$sei
  data <- data[order(data$ESRAW), ]
  data$y <- nrow(data):1
  
  overall <- data.frame(
    estimate = as.numeric(coef(model)),
    ci_low   = model$ci.lb,
    ci_high  = model$ci.ub,
    ypos     = 0
  )
  
  p <- ggplot(data, aes(x = ESRAW, y = y)) +
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
    annotate("text", x = overall$estimate, y = -0.8,
             label = sprintf("Average effect size (Cohen's d) = %.2f [%.2f, %.2f]",
                             overall$estimate, overall$ci_low, overall$ci_high),
             size = 5, hjust = 1.2) +
    labs(x = "Effect size (Cohen's d) with 95% CI", y = NULL) +
    scale_y_reverse() +
    theme_minimal(base_size = 16) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  return(p)
}

# Function to create time trend plot
create_time_plot <- function(data, model) {
  
  years_seq <- sort(unique(data$year))
  pred_list <- predict(model, newmods = years_seq)
  
  pred_data <- data.frame(
    year = years_seq,
    pred = pred_list$pred,
    ci.lb = pred_list$ci.lb,
    ci.ub = pred_list$ci.ub
  )
  
  label_data <- pred_data %>%
    filter(year %in% c(min(year), max(year))) %>%
    mutate(label = gsub("^0", "", sprintf("%.2f", pred)))
  
  p <- ggplot() +
    geom_jitter(data = data, aes(x = year, y = ESRAW),
                width = 0.5, alpha = 0.5, color = "grey60") +
    geom_ribbon(data = pred_data, aes(x = year, ymin = ci.lb, ymax = ci.ub),
                fill = "grey80", alpha = 0.35) +
    geom_line(data = pred_data, aes(x = year, y = pred),
              linewidth = 1.2, color = "black") +
    geom_smooth(data = data, aes(x = year, y = ESRAW),
                method = "loess", se = FALSE, linetype = "dashed",
                linewidth = 0.8, color = "grey30") +
    geom_text(data = label_data, aes(x = year, y = pred, label = label),
              vjust = -1.0, size = 4, fontface = "bold", color = "black") +
    scale_x_continuous(breaks = seq(min(years_seq), max(years_seq), by = 2)) +
    labs(title = "Predicted Effect Sizes Over Time",
         x = "Publication Year", y = "Cohen's d") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text = element_text(color = "black"),
          panel.grid.minor = element_blank())
  
  return(p)
}