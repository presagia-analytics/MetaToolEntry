# noted that the MetaTool module is required to run this app.

library(shiny)
library(DT)
library(rhandsontable)

test_input_org <- readRDS("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/data/test_input.RDS")
show_col <- c('nct_id', 'primary_purpose', 'phase')
test_input <- test_input_org[,show_col]
source(file.path(here::here(), "module/module_ttf.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_num.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_cat.R"), encoding = 'UTF-8')
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/trial-input.r")
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/rawchar.r")


shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

test_input$outcome <- ""
test_input$Action <- shinyInput(actionButton, nrow(test_input), 'button_', label = "Add Outcome", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )



ui <- fluidPage(
    fluidRow(
      column(
        6,
        DT::dataTableOutput("data")
        )
      ,
      column(
        5,
        h5(strong("You are entering the outcome for trial:")), 
        textOutput('myText'),
        hr(),
        # h5(strong("Pick a outcome:")),
        # 
        #   actionButton("OS", "OS"),
        #   actionButton("PFS", "PFS"),
        #   actionButton("RECIST", "RECIST"),
        # 
        # hr(),
        h5(strong("Entry the outcome:")),
        tabsetPanel(
          id = "outcome_tabs",
          type = "tabs",
          tabPanel("OS",
                   ui_ttf("os")
          ),
          tabPanel("PFS",
                   ui_ttf("pfs")
          ),
          tabPanel("RECIST",
                   ui_cat ("RECIST")
          )
        )
        ,
        style = 'border-left: 1px solid'
      )
    ))


server <- function(input, output) {
  
  myValue <- reactiveValues(employee = '')
  values <- reactiveValues()
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  
  output$data <- DT::renderDataTable(
    test_input, server = FALSE, escape = FALSE, selection = 'none'
  )
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$employee <<- paste('',test_input[selectedRow,1])
  })
  
  # output$myText <- renderText({
  #   myValue$employee
  # })
  # 
  # 
  # observeEvent(input$runif, {
  #   v$data <- runif(100)
  # })
  
  server_ttf(id = "os", values)
  server_ttf(id = "pfs",values)
  server_cat("RECIST",values)
  
  output$myText <- renderText({
    
    myValue$employee
    
  })

  
}

shinyApp(ui = ui, server = server)
# runApp(list(ui = ui, server = server), host = "0.0.0.0", port = 80)
