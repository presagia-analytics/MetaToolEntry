library(shiny)
library(rhandsontable)
library(ggplot2)
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_ttf.R"), encoding = 'UTF-8')

options("gargoyle.talkative" = TRUE)

ui <- fluidPage(
  ui_ttf("ttf1"),
  hr(),
  textOutput("text"),
  dataTableOutput('table')
)

server <- function(input, output) {
  values <- reactiveValues()
  df <- read.csv(file.path(here::here(),"tests_data/FirstDataSource.csv"))
  values[['summary_table']] <- df
  server_ttf("ttf1",values)
  output$text <- renderText({
    paste0(names(values),sep = "  //")
  })
  output$table <- renderDataTable(
    #browser()
    values[["all_outcome"]]
  )
}

shinyApp(ui, server)