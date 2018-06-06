library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(lubridate)

theme_set(theme_bw())

source("../../functions.R")
config <- load_config("../../../")

diagnostics <- readRDS(file.path(config$wd, "model-diagnostics.rds"))
calib <- diagnostics$calibration

header <- dashboardHeader(title = "SHEDS Stream Temperature Model - Diagnostics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Calibration", tabName = "calibration", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overview",
      h2("Model Overview"),
      box(
        title = "Model Info",
        paste0("Working Directory: ", config$wd)
      )
    ),
    tabItem(
      tabName = "calibration",
      h2("Model Calibration"),
      fluidRow(
        box(
          title = "Options",
          checkboxInput("includeAR", "Include Autoregressive Term", FALSE),
          verbatimTextOutput("includeARV")
        ),
        box(
          title = "Debug",
          verbatimTextOutput("textCalib")
        )
      ),
      fluidRow(
        box(
          title = "Summary Stats",
          width = 3,
          tableOutput("tblCalibrationStats")
        ),
        box(
          title = "Scatter Plot",
          width = 9,
          plotOutput("plotCalibScatter")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$includeARValue <- renderText({ input$includeAR })

  dataset_calib <- reactive({
    if (input$includeAR) {
      col_value <- "pred"
      col_resid <- "resid"
      col_rmse <- "rmse"
    } else {
      col_value <- "Y"
      col_resid <- "resid_Y"
      col_rmse <- "rmse_Y"
    }
    x <- calib$values[, c("date", "site", "year", "huc8", "temp", col_value, col_resid)]
    names(x)[6:7] <- c("pred", "resid")
    x
  })

  output$textCalib <- renderText({
    df <- dataset_calib()
    nrow(df)
  })

  output$tblCalibrationStats <- renderTable({
    df <- dataset_calib()
    tribble(
      ~label, ~value,
      "# Observations", scales::comma(nrow(df)),
      "RMSE", format(sqrt(mean(df$resid ^ 2)), digits = 3),
      "R^2", format(cor(df$temp, df$pred)^2, digits = 3),
      "Median Abs. Residual", format(median(abs(df$resid)), digits = 3),
      "Mean Residual", format(mean(df$resid), digits = 3),
      "Median Residual", format(median(df$resid), digits = 3),
      "Max Residual", format(max(df$resid), digits = 3),
      "95th Percentile Residual", format(quantile(df$resid, probs = 0.95), digits = 3),
      "5th Percentile Residual", format(quantile(df$resid, probs = 0.05), digits = 3),
      "Min Residual", format(min(df$resid), digits = 3)
    ) %>%
      as_data_frame()
  }, colnames = FALSE, align = "rl")

  output$plotCalibScatter <- renderPlot({
    df <- dataset_calib()
    df %>%
      ggplot(aes(temp, pred)) +
      geom_abline() +
      geom_point(size = 0.5, alpha = 0.25) +
      geom_smooth(method = "lm", se = FALSE) +
      coord_equal() +
      labs(
        x = "Observed Temperature (degC)",
        y = "Predicted Temperature (degC)"
      ) +
      theme(aspect.ratio = 1)
  })

}

shinyApp(ui, server)