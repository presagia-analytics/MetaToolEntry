library(shiny)
library(rhandsontable)
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_num.R"), encoding = 'UTF-8')
ui <- fluidPage(
  ui_num("counter1"),
  hr(),
  textOutput("text"),
  dataTableOutput('table')
)

server <- function(input, output, session) {
  values <- reactiveValues()
  df <- read.csv(file.path(here::here(),"tests_data/FirstDataSource.csv"))
  values[['summary_table']] <- df
  server_num("counter1",values)
  output$text <- renderText({
    paste0(names(values),sep = "  //")
    })
  output$table <- renderDataTable(
    #browser()
    values[["all_outcome"]]
  )

}

shinyApp(ui, server)