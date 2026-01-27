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
          "subgroup_type", "Analysis Type:",
          choices = c(
            "Outcome Variable" = "outcome",
            "Non-XR Comparison" = "comparison",
            "XR Types" = "xr_type"
          ),
          selected = "outcome"
        )
      )
    ),
    
    column(
      width = 9,
      
      conditionalPanel(
        condition = "input.subgroup_type == 'outcome'",
        box(
          title = "Attitudinal Responses (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_att_table"))
        ),
        box(
          title = "Behavioral Responses (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_beh_table"))
        )
      ),
      
      conditionalPanel(
        condition = "input.subgroup_type == 'comparison'",
        box(
          title = "Overall Images (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_img_table"))
        ),
        box(
          title = "Overall Videos (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_vid_table"))
        )
      ),
      
      conditionalPanel(
        condition = "input.subgroup_type == 'xr_type'",
        box(
          title = "AR (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_ar_table"))
        ),
        box(
          title = "VR HMD (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_vrhmd_table"))
        ),
        box(
          title = "VR PC (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_vrpc_table"))
        ),
        box(
          title = "3D (Meta-Regression)", width = NULL,
          status = "primary", solidHeader = TRUE,
          div(style = "width:100%; overflow-x:auto;", DTOutput("subgroup_3d_table"))
        )
      )
    )
  )
)