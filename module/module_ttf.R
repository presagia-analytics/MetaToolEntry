### make ttf module

ui_ttf <- function(id) {
  ns <- NS(id)
  tagList(
    h4(strong("Trial Information")),
    fluidRow(
      column(
        width = 4,
        numericInput(ns("n_arms"), label = h5(strong("Number of Cohorts:")), 7, min = 1, max = 100)),
      column(
        width = 4,
        selectInput(ns("unit_time"), label = h5(strong("Unit of Time in the Study:")), c("month","week","year","day")))
    ),

    fluidRow(
      column(
        width = 4,
        fileInput(ns("img_input"), h5(strong("Upload Figure:")), accept = c('image/png', 'image/jpeg'))),
      column(
        width = 4,
        fileInput(ns("excel_input"),  h5(strong('Upload KM Data:')), accept = c('.csv'),multiple = TRUE)),
      column(
        width = 4,
        fileInput(ns("ipd_input"),  h5(strong('Upload IPD Data:')), accept = c('.csv'),multiple = TRUE))
    ),
    rHandsontableOutput(ns("ttf_table")),
    br(),
    fluidRow(column(12,verticalLayout(actionButton(ns("save_table"), "Save")))),
    hr(),
    h4(strong("Figures")),
    fluidRow(
      column(4,
             h5(strong("Uploaded figure:")),
             h2(""),
             uiOutput(ns("pdffig"))
      ),

      column(4,
             h5(strong("Generated figure:")),
             plotOutput(ns("excfig"))
      ),

      column(4,
             h5(strong("Generated from IPD:")),
             h2(""),
             plotOutput(ns("ipdfig"))
      )
    ),
    hr(),
    h4(strong("Patient Level Data Generator")),
    h5("If you do not have PID data, you can generate IPD data in this section.There are two types of methods for generating IPD data."),
    h5(strong("Generate from Risk Table")),
    h5("The most accurate method is to use the risk table from the survival figure. You must upload the KM-curve data and complete the risk table below."),
    h5(strong("Generate from Estimated Median Survival")),
    h5("If you don't have risk table or KM-curve data, the IPD data will get from the estimated median survival, which requires the total number of patients, the number of events, and the estimated median survival. If you don't know how many events there are, the default is 70% of the total number of patients."),
    br(),
    h5(strong("Number at Risk Table:")),
    fluidRow(column(12,rHandsontableOutput(ns("risk_table")))),
    fluidRow(
      #column(3,verticalLayout(actionButton(ns("use_risktable"), "Use Risk Table")))#,
      #column(3,verticalLayout(actionButton(ns("use_median"), "Use Median Survival")))#,
      column(3,verticalLayout(actionButton(ns("add_ipd"), "Save IPD Data")))
      ),
    h5(strong("IPD Table (Treatment combined):")),
    fluidRow(column(12,rHandsontableOutput(ns("ipd_table"))))
  )
}

server_ttf <- function(id, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      ttf_values <- reactiveValues()
      
      gargoyle::init("make_plot", "make_ipd")

      ## PDF figure
      base64_os <- reactive({
        inFile <- input$img_input
        if(!is.null(inFile)){
          base64enc::dataURI(file = inFile$datapath, mime = "image/png")
        }

      })

      output$pdffig <- renderUI({
        if(!is.null(base64_os())){
          tags$div(
            tags$img(src= base64_os(), width="100%")
          )
        }
      })
      
      ## survival table
      ttf_table_gen <- reactive({
        create_ttf_table(6)
      })
      
      observe({
        if (!is.null(input$ttf_table)) {
          ttf_values[["ttf_table_pre"]] <- isolate(ttf_values[[ns("ttf_table")]])
          DF = hot_to_r(input$ttf_table)
        } else {
          if (is.null(ttf_values[[ns("ttf_table")]]))
            DF <- ttf_table_gen()
          else
            DF <- ttf_values[[ns("ttf_table")]]
        }
        ttf_values[[ns("ttf_table")]] <- DF
      })
      
      
      output$ttf_table <- renderRHandsontable({
        
        if (is.null(ttf_values[[ns("ttf_table")]])){
          res_table  = ttf_table_gen()
        }else{
          res_table = ttf_values[[ns("ttf_table")]]
        }
        
        sumtable <- isolate(app_values[['summary_table']])
        all_outcome <- isolate(app_values[['all_outcome']])
        tr_sub_list <- make_trsub_list(sumtable, all_outcome)
        
        res_table <- adjust_row(res_table, input$n_arms)
        
        out_table <- rhandsontable(res_table ,useTypes = TRUE, selectCallback = TRUE, height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE,  rowHeaderWidth = 0, stretchH = 'all') %>%
          hot_col(col = "ID", readOnly = TRUE) %>%
          hot_col("Treatment",type = "autocomplete", source = tr_sub_list$Treatment , strict = FALSE) %>%
          hot_col("Subgroup",type = "autocomplete", source = tr_sub_list$Subgroup , strict = FALSE) %>%
          hot_col("Pathology",type = "autocomplete", source = tr_sub_list$Pathology, strict = FALSE) %>%
          hot_col("KM.Data",type = "dropdown", source = c("",input$excel_input$name) , strict = TRUE) %>%
          hot_col("IPD.Data",type = "dropdown", source = c("",input$ipd_input$name) , strict = TRUE)
        
        out_table
      })
      
      ## save button
      observeEvent(input$save_table,{
          tmp_df <- isolate(ttf_values[[ns("ttf_table")]])
          rhandtable <- tmp_df
          
          tmp_df$km_data <- NA
          tmp_df$ipd <- NA
          tmp_df$ipd_type <- NA
          tmp_df$fig_path <- NA
          
          if(!is.null(input$excel_input)){
            tmp_df <- 
              tmp_df %>%
                add_km(input$excel_input, input$unit_time) %>%
                update_median()
                rhandtable  <- tmp_df[-which(colnames(tmp_df)%in% c("km_data","ipd"))]
          }
          
          if(!is.null(input$ipd_input)){
            tmp_df <- 
              tmp_df %>%
              add_ipd_upload(input$ipd_input)
          }
          
          if(!is.null(input$img_input)){
            tmp_df$fig_path <- input$img_input$datapath
          }
          
          ttf_values[[ns('final_data')]] <- tmp_df
          ttf_values[[ns('ttf_table')]]  <- rhandtable

          all_outcome <- isolate(app_values[['all_outcome']])
          app_values[['all_outcome']] <- merge_outcome(all_outcome,tmp_df, ns)

          gargoyle::trigger("make_plot")
      })
      
      
      ## make figures 
      output$excfig <- renderPlot({
        #browser()
        gargoyle::watch("make_plot")
        ttf_table <- isolate(ttf_values[[ns("final_data")]])
        try(table2survplot(ttf_table,input$excel_input))
      })
      
      output$ipdfig <- renderPlot({
        gargoyle::watch("make_ipd")
        ipd_table <- isolate(ttf_values[[ns('ipd_table')]])
        if(!is.null(ipd_table)){
          make_ipd_figure(ipd_table)}
      })

      

      #-----
      # IPD generator
      risk_table_gen <- eventReactive(input$img_input,{
        make_risk_table(input$img_input$datapath)
      }) 

      observe({
        if (!is.null(input$risk_table)) {
          ttf_values[["ttf_table_pre"]] <- isolate(ttf_values[[ns("risk_table")]])
          DF = hot_to_r(input$risk_table)
        } else {
          if (is.null(ttf_values[[ns("risk_table")]])){
            DF <- risk_table_gen()
          }
          else
            DF <- ttf_values[[ns("risk_table")]]
        }
        ttf_values[[ns("risk_table")]] <- DF
      })

      output$risk_table  <- renderRHandsontable({

        if (is.null(ttf_values[[ns("risk_table")]])){
          risk_table  = risk_table_gen()
        }else{
          risk_table = ttf_values[[ns("risk_table")]]
        }

        no_arm = input$n_arms + 1
        ttf_table = ttf_values[[ns("ttf_table")]]

        risk_table <- adjust_row(risk_table, no_arm)[colnames(risk_table)]

        out_table <- rhandsontable(risk_table ,useTypes = TRUE, selectCallback = TRUE, height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE,  rowHeaderWidth = 0, stretchH = 'all') %>%
          hot_col("Treatment",type = "dropdown", source = sort(unique(ttf_table$Treatment))) %>%
          hot_col("Subgroup",type = "dropdown", source = sort(unique(ttf_table$Subgroup))) %>%
          hot_col("Pathology",type = "dropdown", source = sort(unique(ttf_table$Pathology))) 

        out_table

      })

      observeEvent(input$use_risktable,{
        final_data <- isolate(ttf_values[[ns('final_data')]])
        risk_table <- isolate(ttf_values[[ns("risk_table")]])

        final_data <- add_ipd_risktable(final_data, risk_table, input$ipd_input, input$unit_time)

        ttf_values[[ns('final_data')]] <- make_final_table(final_data, ns)
        ttf_values[[ns('ipd_table')]] <- do.call(rbind,final_data$ipd)
        gargoyle::trigger("make_ipd")
      })
      
      
      
      

      output$ipd_table <- renderRHandsontable({
        gargoyle::watch("make_ipd")
        ipd_table <- isolate(ttf_values[[ns('ipd_table')]])
        if(!is.null(ipd_table)){
        out_table <- rhandsontable(ipd_table  ,useTypes = TRUE, selectCallback = TRUE, height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE,  rowHeaderWidth = 0, stretchH = 'all')
        out_table
        }
      })




    }
  )
}


library(shiny)
library(rhandsontable)
library(ggplot2)
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')

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
