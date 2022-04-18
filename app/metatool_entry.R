library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(shinydashboard)
library(rhandsontable)
library(dplyr)
library(rmarkdown)
library(shinyFeedback)
library(here)


source(file.path(here::here(), "module/module_ttf.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_num.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_cat.R"), encoding = 'UTF-8')
source(file.path(here::here(), "module/module_summary.R"), encoding = 'UTF-8')
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')

ui <-navbarPage("MetaTool",
                collapsible = TRUE,
                inverse = FALSE,
                theme = shinythemes::shinytheme("yeti"),

                tabPanel("Data Entry",
                         #shinyalert::useShinyalert(force = TRUE),
                         fluidPage(
                           #tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                           #tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px
                           #!important; color: blue;font-size: 20px;font-style: italic;}"),
                           sidebarLayout(
                             sidebarPanel(
                               tags$style(type='text/css', " { height:30px }"),
                               width = 3,
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
                ),
                tabPanel("Study Summary",
                         ui_summary ("ss"))

)



server <- function(input, output,session) {
  values <- reactiveValues()
  metaData <- reactiveValues()
  values[["all_outcome"]] <- NULL

  ## Set UP ####
  observe({
    ### Load Existing Data ####
    if(file.exists("raw-data/AtezSource_demo.rds")){
      ddf <- readRDS("raw-data/AtezSource_demo.rds")
      outcome_names_all <- stringr::str_remove(colnames(ddf)[grepl('\\b.N\\b',colnames(ddf))], ".N")
      outcome_matrix <- !is.na(ddf[colnames(ddf)[grepl('.N\\b',colnames(ddf))]])
      ddf$Outcome <- apply(outcome_matrix, 1, function(x) paste0(outcome_names_all[x],collapse = '; '))
      values[['rds']] <- ddf

      updateSelectInput(session, "Type", choices = ddf$Type)


    }else{
      col_names <- c("ID","PID","PaperName","FirstAuthor","NCT","Year", "Type","Phase","TrLine", "Treatment","Subgroup")
      ddf <- data.frame(matrix(ncol = length(col_names),nrow = 0))
      colnames(ddf) <- col_names
      values[['rds']]  <- NULL
    }

    ddf<-as.data.frame(lapply(ddf, as.character))
    values[['summary_table']] <- ddf



    ### Folder ####

    if(!dir.exists("extractedData")){
      dir.create("extractedData")
    }

    if(!dir.exists("kmFigures")){
      dir.create("kmFigures")
    }

    if(!dir.exists("paper")){
      dir.create("paper")
    }

  })

  # Data Selection####
  server_summary("ss",values)

  # Mata Analysis ####
  #server_metaanalysis("MetaAna", metaData, values[['Sel_Table']])


  ## Data Entry ####
  ## Read PDF paper
  DD <- reactive({
    req(input$pdf_input)
    ptable <- values[['summary_table']]
    updateTextInput(session, "PID", value = max(as.numeric(ptable$PID),0)+1)


    txt <- pdftools::pdf_text(input$pdf_input$datapath)
    NCT_index <- stringr::str_locate(txt[1],"NCT")
    NCT_num   <- substr(txt[1],NCT_index[1], NCT_index[1]+10)
    updateTextInput(session, "NCT", value = NCT_num)

    # Author, version, etc
    info <- pdftools::pdf_info(input$pdf_input$datapath)
    firstauthor <- strsplit(info$keys$Author, ",")[[1]][1]
    updateTextInput(session, "FirstAuthor", value = firstauthor)

    pubyear <- format(as.Date(info$created),"%Y")
    updateTextInput(session, "Year", value = pubyear)

    Title <- info$keys$Title[1]
  })

  output$TitleName <- renderText({DD()})

  ### Add Tabs ####
  #### 1. ttf ####
  observeEvent(input$addTTF, {
    req(input$outcome_name)
    outcome_name = input$outcome_name

    insertTab(inputId = "outcome_tabs",  ## input Id is the tab series name that was set up in main UI
              tabPanel(outcome_name,            ## this is the tab name of this tab, that should be changed based on the input.
                       ui_ttf(outcome_name)),  ## this should the same name asthe tab, used to save in values.
              target = 'PFS',
              position = c("after")
    )
    server_ttf(id =outcome_name,values = values)   ## this should be the same name as the tab and ui
    updateTextInput(session, "outcome_name", value = "")
  })


  #### 2. num ####
  observeEvent(input$addNum, {
    req(input$outcome_name)
    outcome_name = input$outcome_name

    insertTab(inputId = "outcome_tabs",
              tabPanel(outcome_name,
                       ui_num(outcome_name)),
              target = 'RECIST'
    )
    server_num(outcome_name,values)
    updateTextInput(session, "outcome_name", value = "")
  })
  #### 3. cat ####
  observeEvent(input$addCat, {
    req(input$outcome_name)
    outcome_name = input$outcome_name
    insertTab(inputId = "outcome_tabs",
              tabPanel(outcome_name,
                       ui_cat(outcome_name)),
              target = 'RECIST',
              position = "after"
    )
    server_cat(outcome_name,values)
    updateTextInput(session, "outcome_name", value = "")

  })

  ### default
  server_ttf(id = "os", values)
  server_ttf(id = "pfs",values)
  server_cat("RECIST",values)

  observeEvent(input$save_rds, {

    combined_df   <- isolate(values[["all_outcome"]])
    source_rds <- values[['rds']]
    study_df<- merge_trial_info(combined_df, reactiveValuesToList(input))
    source_rds <- bind_rows(source_rds,study_df)

    source_rds$ID    <- seq(1:nrow(source_rds))
    source_rds$tr_id <- paste0(source_rds$ID,"-" ,source_rds$Treatment)

    n1 = paste(paste("Source",sep = "_"),"RDS", sep = ".")
    n2 = paste(paste("Source",sep = "_"),"csv", sep = ".")

    saveRDS(source_rds , file = n1)
    shinyalert::shinyalert("Download", "Data Saved", type = "success")

  })


  ### Download to folders ####

  session$onSessionEnded(function() {
    stopApp()
  })

  ### END

}

shinyApp(ui = ui, server = server)
#runApp(list(ui = ui, server = server))
