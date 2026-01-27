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
          "time_analysis", "Analysis Type:",
          choices = c(
            "Time Effects Overall" = "time_overall",
            "Time by XR & Comparison" = "time_subgroup",
            "XR Familiarity Overall" = "fam_overall",
            "XR Familiarity by XR Type" = "fam_xr",
            "XR Familiarity by Comparison" = "fam_comp"
          ),
          selected = "time_overall"
        )
      )
    ),
    
    column(
      width = 9,
      box(
        title = "Time & Familiarity Effects", width = NULL, status = "primary", solidHeader = TRUE,
        div(
          style = "width:100%; overflow-x:auto; padding-left:12px; padding-right:12px;",
          plotOutput("time_plot", height = "600px", width = "100%")
        )
      ),
      
      box(
        title = "Results Table", width = NULL, status = "info", solidHeader = TRUE,
        collapsible = TRUE,
        div(style = "width:100%; overflow-x:auto;", DTOutput("time_table"))
      )
    )
  )
)