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

      /* --- Forest plot: prevent left-side clipping of caption/text --- */
      /* Center the rendered plot image/canvas inside its container */
      #forest_plot img, #forest_plot canvas {
        display: block !important;
        margin-left: auto !important;
        margin-right: auto !important;
      }
    "))
  ),
  
  fluidRow(
    column(
      width = 3,
      box(
        title = "Filters", width = NULL, status = "primary", solidHeader = TRUE,
        
        selectInput(
          "filter_type", "Filter By:",
          choices = c(
            "Overall" = "overall",
            "XR Type" = "xr_type",
            "Comparison Condition" = "comparison",
            "Outcome Variable" = "outcome",
            "XR x Comparison x Outcome" = "interaction",
            "Correlational Studies" = "correlational"
          ),
          selected = "overall"
        ),
        
        conditionalPanel(
          condition = "input.filter_type == 'xr_type'",
          checkboxGroupInput(
            "xr_types", "Select XR Types:",
            choices = c(
              "VR HMD" = "VRHMD",
              "VR PC" = "VRPC",
              "AR" = "AR",
              "3D" = "ThreeD"
            ),
            selected = c("VRHMD", "VRPC", "AR", "ThreeD")
          )
        ),
        
        conditionalPanel(
          condition = "input.filter_type == 'comparison'",
          checkboxGroupInput(
            "comparison_types", "Select Comparison:",
            choices = c(
              "Overall Images" = "overall_img",
              "Images Only" = "img_only",
              "Images + Text" = "img_text",
              "Overall Videos" = "overall_vid",
              "Videos Only" = "vid_only",
              "Videos + Text" = "vid_text"
            ),
            selected = c("overall_img", "overall_vid")
          )
        ),
        
        conditionalPanel(
          condition = "input.filter_type == 'outcome'",
          checkboxGroupInput(
            "outcome_types", "Select Outcome:",
            choices = c(
              "Attitudinal Responses" = "attitude",
              "Behavioral Intentions" = "intention",
              "Behavior" = "behavior",
              "Behavioral Responses" = "behavioral"
            ),
            selected = c("attitude", "behavioral")
          )
        ),
        
        conditionalPanel(
          condition = "input.filter_type == 'interaction'",
          selectInput(
            "interaction_type", "Select Analysis:",
            choices = c(
              "XRs vs. Images - Attitudinal" = "xr_img_att",
              "XRs vs. Images - Behavioral" = "xr_img_beh",
              "XRs vs. Videos - Attitudinal" = "xr_vid_att",
              "XRs vs. Videos - Behavioral" = "xr_vid_beh",
              "AR vs. Non-XRs - Attitudinal" = "ar_nonxr_att",
              "AR vs. Non-XRs - Behavioral" = "ar_nonxr_beh",
              "AR vs. Images - Overall" = "ar_img_overall",
              "AR vs. Images - Attitudinal" = "ar_img_att",
              "AR vs. Images - Behavioral" = "ar_img_beh",
              "AR vs. Videos - Overall" = "ar_vid_overall",
              "AR vs. Videos - Attitudinal" = "ar_vid_att",
              "AR vs. Videos - Behavioral" = "ar_vid_beh",
              "VR HMD vs. Non-XRs - Attitudinal" = "vrhmd_nonxr_att",
              "VR HMD vs. Non-XRs - Behavioral" = "vrhmd_nonxr_beh",
              "VR HMD vs. Images - Overall" = "vrhmd_img_overall",
              "VR HMD vs. Images - Attitudinal" = "vrhmd_img_att",
              "VR HMD vs. Images - Behavioral" = "vrhmd_img_beh",
              "VR HMD vs. Videos - Overall" = "vrhmd_vid_overall",
              "VR HMD vs. Videos - Attitudinal" = "vrhmd_vid_att",
              "VR HMD vs. Videos - Behavioral" = "vrhmd_vid_beh",
              "VR PC vs. Non-XRs - Attitudinal" = "vrpc_nonxr_att",
              "VR PC vs. Non-XRs - Behavioral" = "vrpc_nonxr_beh",
              "VR PC vs. Images - Overall" = "vrpc_img_overall",
              "VR PC vs. Images - Attitudinal" = "vrpc_img_att",
              "VR PC vs. Images - Behavioral" = "vrpc_img_beh",
              "VR PC vs. Videos - Overall" = "vrpc_vid_overall",
              "VR PC vs. Videos - Attitudinal" = "vrpc_vid_att",
              "VR PC vs. Videos - Behavioral" = "vrpc_vid_beh",
              "3D vs. Non-XRs - Attitudinal" = "3d_nonxr_att",
              "3D vs. Non-XRs - Behavioral" = "3d_nonxr_beh",
              "3D vs. Images - Overall" = "3d_img_overall",
              "3D vs. Images - Attitudinal" = "3d_img_att",
              "3D vs. Images - Behavioral" = "3d_img_beh",
              "3D vs. Videos - Overall" = "3d_vid_overall",
              "3D vs. Videos - Attitudinal" = "3d_vid_att",
              "3D vs. Videos - Behavioral" = "3d_vid_beh"
            ),
            selected = "xr_img_att"
          )
        ),
        
        conditionalPanel(
          condition = "input.filter_type == 'correlational'",
          selectInput(
            "corr_filter", "Select Analysis:",
            choices = c(
              "Overall" = "overall",
              "Attitudinal Responses" = "attitude",
              "Behavioral Responses" = "behavioral",
              "AR" = "AR",
              "VR HMD" = "VRHMD",
              "VR PC" = "VRPC",
              "3D" = "ThreeD"
            ),
            selected = "overall"
          )
        )
      )
    ),
    
    column(
      width = 9,
      box(
        title = "Results", width = NULL, status = "primary", solidHeader = TRUE,
        
        conditionalPanel(
          condition = "input.filter_type == 'overall'",
          div(
            style = "width:100%; overflow-x:auto;",
            div(
              style = "min-width: 920px; padding-left: 28px; padding-right: 16px; box-sizing: border-box;",
              plotOutput("forest_plot", height = "600px", width = "100%")
            )
          )
        ),
        
        div(style = "width:100%; overflow-x:auto;", DTOutput("main_effects_table")),
        
        conditionalPanel(
          condition = "input.filter_type == 'xr_type' || input.filter_type == 'comparison' || input.filter_type == 'outcome'",
          hr(),
          h4("Pairwise Comparisons"),
          div(style = "width:100%; overflow-x:auto;", DTOutput("pairwise_table"))
        )
      ),
      
      box(
        title = "Dataset Description", width = NULL, status = "info", solidHeader = TRUE,
        collapsible = TRUE, collapsed = FALSE,
        div(style = "width:100%; overflow-x:auto;", DTOutput("dataset_description"))
      )
    )
  )
)