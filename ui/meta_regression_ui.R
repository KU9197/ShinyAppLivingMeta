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
        
        radioButtons(
          "study_type_meta", "Study Type:",
          choices = c(
            "Experimental Studies" = "exp",
            "Descriptives Experimental" = "desc_exp",
            "Correlational Studies" = "corr"
          ),
          selected = "exp"
        )
      )
    ),
    
    column(
      width = 9,
      box(
        title = "Meta-Regression Results", width = NULL, status = "primary", solidHeader = TRUE,
        div(style = "width:100%; overflow-x:auto;", DTOutput("meta_regression_table"))
      ),
      
      conditionalPanel(
        condition = "input.study_type_meta == 'exp'",
        box(
          title = "Predicted Values", width = NULL, status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("predicted_values_table"))
        )
      ),
      
      conditionalPanel(
        condition = "input.study_type_meta == 'desc_exp'",
        box(
          title = "Descriptive Statistics (Binary)", width = NULL, status = "info", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("descriptives_binary_table"))
        ),
        box(
          title = "Descriptive Statistics (Continuous)", width = NULL, status = "info", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("descriptives_continuous_table"))
        )
      )
    )
  )
)