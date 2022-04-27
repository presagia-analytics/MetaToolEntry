library(shiny)
library(DT)
library(rhandsontable)
library(gargoyle)
library(ggplot2)

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
colnames(test_input) <- c("NCT","Title","Outcome","Action")

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
                            shinyjs::useShinyjs(),
                            div(
                              id = "entry_tab",
                            sidebarLayout(
                              sidebarPanel(
                                tags$style(type='text/css', " { height:30px }"),
                                width = 3,
                                h5(strong("NCT number:")), 
                                #textOutput('sel_nct'),
                                verbatimTextOutput('sel_nct'),
                                fileInput('pdf_input', h5(strong('Upload Pdf')), accept = c('.pdf')),
                                shinysky::select2Input("Type",label = h5(strong("Cancer Type")),choices=c("Bladder"),selected=c("")),
                                shinysky::select2Input("Phase",label = h5(strong("Phase")),choices=c("1","2","3","4","Unknown"),selected=c("")),
                                shinysky::select2Input("TrLine",label = h5(strong("Therapy Lines")),choices=c("1","2","3","4"),selected=c("")),
                                textInput("Year", label = h5(strong("Year")), value = " "),
                                textInput("FirstAuthor", label = h5(strong("Author First Name")), value = " "),
                                textInput("LastAuthor", label = h5(strong("Author Last Name")), value = " "),
                                hr(),
                                actionButton("jumpToP1", "Save Results")
                              ),
                              # Create a spot for the barplot
                              mainPanel(
                                width = 9,
                                
                                h4(strong("Paper Title:")),
                                verbatimTextOutput("sel_paper_name"),
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
                          ))
                          
                          
                          
                 )
)

server <- function(input, output, session) {
  
  trial_value <- reactiveValues(employee = '')
  entry_value <- reactiveValues()

  
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
    trial_value$nct <<- paste('',test_input[selectedRow,1])
    trial_value$paper_name <<- paste('',test_input[selectedRow,2])
    reload_module <- TRUE
    shinyjs::reset("entry_tab")
    shinyjs::reset("Type")
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  
  
  #### Tab2

  server_ttf(id = "os", entry_value)
  server_ttf(id = "pfs",entry_value)
  server_cat("RECIST",entry_value)
  
  output$sel_nct <- renderText({
    trial_value$nct
  })
  
  output$sel_paper_name <- renderText({
    trial_value$paper_name
  })
  
  
  #### Save and back to first Tab
  observeEvent(input$jumpToP1, {
    browser()
    combined_df <- isolate(entry_value[["all_outcome"]])
    input_info <- reactiveValuesToList(input)
   
    for (row_id in seq(1:nrow(combined_df))){
      doce_outcome_list <- make_doce_outcome(combined_df[row_id,],input_info)
      single_trial <- make_trial(input,doce_outcome_list,pub,trial_value)
      write_trial(single_trial, trial_con())0116
    }
    
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  
}

shinyApp(ui, server)
