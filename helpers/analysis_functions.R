library(metafor)
library(dplyr)
library(tibble)

# Function to run meta-analysis
run_meta_analysis <- function(data, yi = "ESRAW", V = "ES_VAR") {
  
  res <- rma.mv(
    yi = data[[yi]],
    V = data[[V]],
    random = ~1 | IDd/IDES,
    data = data,
    method = "REML"
  )
  
  # Summary statistics
  N_sample <- data[!duplicated(data$IDd), ]
  
  result <- list(
    model = res,
    estimate = as.numeric(res$b),
    ci_lb = res$ci.lb,
    ci_ub = res$ci.ub,
    se = res$se,
    pval = res$pval,
    n_effects = nrow(data),
    n_studies = n_distinct(data$IDd),
    n_papers = n_distinct(data$IDp),
    total_sample = sum(N_sample$ES_Sample, na.rm = TRUE)
  )
  
  return(result)
}

# Format results as data frame
# NOTE: Keeps existing Shiny column names, but computes SE from CI like in the original MainEffects script.
format_results <- function(result, label = "Overall") {
  se_from_ci <- (as.numeric(result$ci_ub) - as.numeric(result$ci_lb)) / (2 * 1.96)
  
  data.frame(
    Analysis = label,
    Estimate = round(as.numeric(result$estimate), 3),
    SE = round(as.numeric(se_from_ci), 3),
    CI_Lower = round(as.numeric(result$ci_lb), 3),
    CI_Upper = round(as.numeric(result$ci_ub), 3),
    p_value = format.pval(as.numeric(result$pval), digits = 3),
    N_Effects = result$n_effects,
    N_Studies = result$n_studies,
    stringsAsFactors = FALSE
  )
}

# Pairwise comparison function (kept for compatibility)
pairwise_compare <- function(ES1, SE1, ES2, SE2) {
  z <- (ES1 - ES2) / sqrt(SE1^2 + SE2^2)
  p <- 2 * (1 - pnorm(abs(z)))
  ES_diff <- ES1 - ES2
  return(c(ES_diff = ES_diff, z = z, p = p))
}

# Pairwise comparisons table following the ORIGINAL MainEffects script logic:
# - SE computed from CI: (CI_Upper - CI_Lower) / (2*1.96)
# - z test on difference in subgroup estimates
# Expects a summary df produced by format_results() (i.e., with Analysis/Estimate/CI_Lower/CI_Upper).
make_pairwise_table_original <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) < 2) {
    return(data.frame(
      Comparison = character(),
      ES_diff = numeric(),
      z = numeric(),
      p = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  required_cols <- c("Analysis", "Estimate", "CI_Lower", "CI_Upper")
  if (!all(required_cols %in% names(summary_df))) {
    stop(
      "make_pairwise_table_original(): summary_df must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  
  summary_df <- summary_df %>%
    mutate(SE_from_CI = (as.numeric(CI_Upper) - as.numeric(CI_Lower)) / (2 * 1.96))
  
  combos <- combn(seq_len(nrow(summary_df)), 2)
  
  out <- lapply(seq_len(ncol(combos)), function(i) {
    idx <- combos[, i]
    
    g1 <- summary_df$Analysis[idx[1]]
    g2 <- summary_df$Analysis[idx[2]]
    
    ES1 <- as.numeric(summary_df$Estimate[idx[1]])
    SE1 <- as.numeric(summary_df$SE_from_CI[idx[1]])
    ES2 <- as.numeric(summary_df$Estimate[idx[2]])
    SE2 <- as.numeric(summary_df$SE_from_CI[idx[2]])
    
    z <- (ES1 - ES2) / sqrt(SE1^2 + SE2^2)
    p <- 2 * (1 - pnorm(abs(z)))
    
    data.frame(
      Comparison = paste(g1, "vs", g2),
      ES_diff = round(ES1 - ES2, 3),
      z = round(z, 3),
      p = format.pval(p, digits = 3),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}

# Generate dataset description
generate_dataset_description <- function(data) {
  N_sample <- data %>% distinct(IDd, .keep_all = TRUE)
  N_paper  <- data %>% distinct(IDp, .keep_all = TRUE)
  
  if ("Pub_Type" %in% colnames(data)) {
    data_paper_only <- data %>% filter(Pub_Type == "paper")
  } else {
    data_paper_only <- data
  }
  
  num_papers      <- n_distinct(data$IDp)
  num_studies     <- n_distinct(data$IDd)
  num_effects     <- n_distinct(data$IDES)
  pub_year_range  <- paste0(min(data$year), "–", max(data$year))
  academic_fields <- paste0(
    names(table(N_paper$academic_field_text)),
    " (n = ", table(N_paper$academic_field_text), ")",
    collapse = "; "
  )
  
  if ("journal" %in% colnames(data_paper_only)) {
    num_journals <- n_distinct(data_paper_only$journal)
  } else {
    num_journals <- NA
  }
  
  if ("Pub_Type" %in% colnames(N_paper)) {
    pub_types <- paste0(
      names(table(N_paper$Pub_Type)), " = ",
      table(N_paper$Pub_Type),
      collapse = "; "
    )
  } else {
    pub_types <- NA
  }
  
  total_sample    <- sum(N_sample$ES_Sample, na.rm = TRUE)
  sample_range    <- paste0(
    min(N_sample$ES_Sample, na.rm = TRUE), "–",
    max(N_sample$ES_Sample, na.rm = TRUE)
  )
  median_sample   <- median(N_sample$ES_Sample, na.rm = TRUE)
  
  if ("country" %in% colnames(data)) {
    num_countries <- n_distinct(data$country)
  } else {
    num_countries <- NA
  }
  
  summary_table <- tibble(
    Characteristic = c(
      "Number of papers",
      "Number of studies",
      "Number of effect sizes",
      "Publication years (range)",
      "Academic fields",
      "Number of different journals",
      "Publication types",
      "Total sample size",
      "Sample size range",
      "Median sample size",
      "Number of countries"
    ),
    Value = c(
      num_papers,
      num_studies,
      num_effects,
      pub_year_range,
      academic_fields,
      num_journals,
      pub_types,
      total_sample,
      sample_range,
      median_sample,
      num_countries
    )
  )
  
  return(summary_table)
}

# Run meta-regression (aligned to original script approach)
run_meta_regression <- function(data, moderators, yi = "ESRAW", V = "ES_VAR") {
  # Work on a local copy so model prep (imputation/centering/type conversions)
  # can't mutate cached/shared data frames used by other outputs.
  data <- as.data.frame(data, check.names = FALSE)
  
  # Mean imputation for mean_age if needed (original script does this widely)
  if ("mean_age" %in% moderators && "mean_age" %in% colnames(data)) {
    if (any(is.na(data$mean_age))) {
      data$mean_age[is.na(data$mean_age)] <- mean(data$mean_age, na.rm = TRUE)
    }
  }
  
  # Mean-center continuous variables (original uses *_MC)
  continuous_vars <- c("hedonic_utilitarian_product", "fit_uncertainty",
                       "uncertainty_avoidance", "mean_age", "year")
  
  for (var in continuous_vars) {
    if (var %in% moderators && var %in% colnames(data)) {
      mc_name <- paste0(var, "_MC")
      data[[mc_name]] <- scale(data[[var]], center = TRUE, scale = FALSE)
    }
  }
  
  # Convert binary 0/1-coded moderators to numeric (prevents factor dummy column name issues)
  for (m in moderators) {
    if (m %in% colnames(data)) {
      vals <- unique(na.omit(as.character(data[[m]])))
      if (length(vals) > 0 && all(vals %in% c("0", "1", "0.0", "1.0"))) {
        data[[m]] <- as.numeric(as.character(data[[m]]))
      }
    }
  }
  
  # Update moderator names for mean-centered vars
  moderators <- gsub("\\bhedonic_utilitarian_product\\b", "hedonic_utilitarian_product_MC", moderators)
  moderators <- gsub("\\bfit_uncertainty\\b", "fit_uncertainty_MC", moderators)
  moderators <- gsub("\\buncertainty_avoidance\\b", "uncertainty_avoidance_MC", moderators)
  moderators <- gsub("\\bmean_age\\b", "mean_age_MC", moderators)
  moderators <- gsub("\\byear\\b", "year_MC", moderators)
  
  # Build formula
  formula_str <- paste(yi, "~", paste(moderators, collapse = " + "))
  
  model <- rma.mv(
    yi = data[[yi]],
    V  = data[[V]],
    random = ~1 | IDd/IDES,
    data = data,
    method = "REML",
    mods = as.formula(formula_str)
  )
  
  return(model)
}
