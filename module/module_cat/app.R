
library(shiny)
library(rhandsontable)
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_cat.R"), encoding = 'UTF-8')
ui <- fluidPage(
  ui_cat ("counter1"),
  hr(),
    textOutput("text"),
    dataTableOutput('table')
  )

server <- function(input, output, session) {
  values <- reactiveValues()
  df <- read.csv(file.path(here::here(),"tests_data/FirstDataSource.csv"))
  values[['summary_table']] <- df
  values[["all_outcome"]] <- NULL
  server_cat("counter1",values)
  output$text <- renderText({
    paste0(names(values),sep = "  //")
  })
  output$table <- renderDataTable(
    #browser()
    values[["all_outcome"]]
    )
}

shinyApp(ui, server)
