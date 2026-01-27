library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(metafor)
library(readxl)

# Source helper files
#source("helpers/data_prep_experimental.R")
#source("helpers/data_prep_correlational.R")
source("helpers/data_prep_mediators.R")
source("helpers/analysis_functions.R")
source("helpers/plot_functions.R")

exp_cache  <- readRDS("data/cache/exp_cache.rds")
corr_cache <- readRDS("data/cache/corr_cache.rds")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Meta-Analysis on Extended Realities"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Effects", tabName = "main_effects", icon = icon("chart-bar")),
      menuItem("Full Meta-Regressions", tabName = "meta_regression", icon = icon("calculator")),
      menuItem("Subgroup Analyses", tabName = "subgroup", icon = icon("filter")),
      menuItem("Time & Familiarity", tabName = "time_effects", icon = icon("clock")),
      menuItem("Robustness Checks", tabName = "robustness", icon = icon("check"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Main Effects Tab
      tabItem(tabName = "main_effects",
              source("ui/main_effects_ui.R", local = TRUE)$value
      ),
      
      # Meta-Regression Tab
      tabItem(tabName = "meta_regression",
              source("ui/meta_regression_ui.R", local = TRUE)$value
      ),
      
      # Subgroup Tab
      tabItem(tabName = "subgroup",
              source("ui/subgroup_ui.R", local = TRUE)$value
      ),
      
      # Time Effects Tab
      tabItem(tabName = "time_effects",
              source("ui/time_effects_ui.R", local = TRUE)$value
      ),
      
      # Robustness Tab
      tabItem(tabName = "robustness",
              source("ui/robustness_ui.R", local = TRUE)$value
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  
  # Provide cached datasets as reactives for all modules
  exp_data <- reactive(exp_cache)
  corr_data <- reactive(corr_cache)
  
  
  # Source server modules
  source("server/main_effects_server.R", local = TRUE)
  source("server/meta_regression_server.R", local = TRUE)
  source("server/subgroup_server.R", local = TRUE)
  source("server/time_effects_server.R", local = TRUE)
  source("server/robustness_server.R", local = TRUE)
}
shinyApp(ui = ui, server = server)