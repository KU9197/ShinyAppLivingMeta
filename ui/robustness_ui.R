tagList(
  tags$head(
    tags$style(HTML("
      /* Make DT tables more compact */
      table.dataTable thead th, table.dataTable thead td {
        padding: 6px 8px !important;
      }
      table.dataTable tbody th, table.dataTable tbody td {
        padding: 4px 8px !important;
      }
      table.dataTable {
        font-size: 12px !important;
      }
    "))
  ),
  
  fluidRow(
    column(
      width = 3,
      box(
        title = "Filters", width = NULL, status = "primary", solidHeader = TRUE,
        
        selectInput(
          "robustness_type", "Robustness Check:",
          choices = c(
            "Univariate Meta-Regressions" = "univariate",
            "Full Model with Outliers" = "with_outliers",
            "Full Model 3SD Outliers" = "3sd_outliers",
            "Theoretical Moderators Only" = "theoretical",
            "Additional Control Variables" = "controls",
            "Reliability Adjustment" = "reliability",
            "Excluding Overlay Studies" = "no_overlay",
            "Median Imputation for Age" = "median_age"
          ),
          selected = "univariate"
        )
      )
    ),
    
    column(
      width = 9,
      box(
        title = "Robustness Check Results", width = NULL, status = "primary", solidHeader = TRUE,
        div(style = "width:100%; overflow-x:auto;", DTOutput("robustness_table"))
      )
    )
  )
)