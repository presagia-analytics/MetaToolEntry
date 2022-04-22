library(shiny)
library(DT)
library(rhandsontable)

test_input_org <- readRDS("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/data/test_input.RDS")
show_col <- c('nct_id', 'official_title')
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
                 tabPanel(title = "CTgov Trials", value = "panel1", 
                          DT::dataTableOutput("data")),
                 #actionButton('jumpToP2', 'Jump to Second Tab')),
                 tabPanel(title = "Outcome Entry", value = "panel2", 
   
                          
                          fluidPage(
                            #tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                            #tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px
                            #!important; color: blue;font-size: 20px;font-style: italic;}"),
                            sidebarLayout(
                              sidebarPanel(
                                tags$style(type='text/css', " { height:30px }"),
                                width = 3,
                                h5(strong("You are entering the outcome for trial:")), 
                                textOutput('myText'),
                                actionButton('jumpToP1', 'Save All'),
                                fileInput('pdf_input', h5(strong('Upload Pdf')), accept = c('.pdf')),
                                h5(strong("Add a New Outcome")),
                                shinysky::select2Input("outcome_name",label = h6(strong("Name (required):")),choices=c("resp"),selected=c(""),
                                                       multiple = FALSE,  size = 26),
                                h6(strong("Type:")),
                                fluidRow(
                                  column(12,
                                         actionButton("addTTF", "TTF"),
                                         actionButton("addNum", "Numeric"),
                                         actionButton("addCat", "Categorical"))),
                                br(),
                                textInput("PID", label = h5(strong("PID")), value = " "),
                                textInput("PaperNickName", label = h5(strong("Paper NickName")), value = " "),
                                shinysky::select2Input("Type",label = h5(strong("Cancer Type")),choices=c("Bladder"),selected=c("")),
                                shinysky::select2Input("Phase",label = h5(strong("Phase")),choices=c("1","2","3","4","Unknown"),selected=c("")),
                                shinysky::select2Input("TrLine",label = h5(strong("Therapy Lines")),choices=c("1","2","3","4"),selected=c("")),
                                textInput("NCT", label = h5(strong("NCT number")), value = " "),
                                textInput("Year", label = h5(strong("Year")), value = " "),
                                textInput("FirstAuthor", label = h5(strong("First Author")), value = " "),
                                hr(),
                                actionButton("save_rds", "Save Results")
                              ),
                              # Create a spot for the barplot
                              mainPanel(
                                width = 9,
                                
                                h4(strong("Paper Title:")),
                                verbatimTextOutput("TitleName"),
                                hr(),
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
                              )
                            )
                          )
                          
                          
                          
                 )
)

server <- function(input, output, session) {
  
  myValue <- reactiveValues(employee = '')
  values <- reactiveValues()
  
  #### Tab1
  output$data <- DT::renderDataTable(
    test_input, 
    server = FALSE, 
    escape = FALSE, 
    selection = 'none',
    options = list(bPaginate = FALSE, searching = FALSE, info = FALSE)
  )
  
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$employee <<- paste('',test_input[selectedRow,1])
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  
  
  #### Tab2

  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  
  output$myText <- renderText({
    myValue$employee
  })
  
  
}

shinyApp(ui, server)
