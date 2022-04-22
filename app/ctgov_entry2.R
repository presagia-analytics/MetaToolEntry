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


ui <- navbarPage('MetaTool Entry',
                 id = "inTabset",
                 collapsible = TRUE,
                 inverse = FALSE,
                 theme = shinythemes::shinytheme("yeti"),
                 tabPanel(title = "Panel 1", value = "panel1", 
                          DT::dataTableOutput("data")),
                          #actionButton('jumpToP2', 'Jump to Second Tab')),
                 tabPanel(title = "Panel 2", value = "panel2", 
                          h5(strong("You are entering the outcome for trial:")), 
                          textOutput('myText'),
                          actionButton('jumpToP1', 'Jump to First Tab'))
)

server <- function(input, output, session) {
  
  myValue <- reactiveValues(employee = '')
  values <- reactiveValues()
  
  output$data <- DT::renderDataTable(
    test_input, server = FALSE, escape = FALSE, selection = 'none'
  )
  
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$employee <<- paste('',test_input[selectedRow,1])
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  
  output$myText <- renderText({
    
    myValue$employee
    
  })
  
}

shinyApp(ui, server)
