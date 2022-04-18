

ui_cat <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             numericInput(ns("n_arms"), label = h5(strong("Number of Cohorts:")), 7, min = 1, max = 100)),
      column(6,
             textInput(ns("cat_level_name"), label = h5(strong("Level of the Outcome")), value = "CR;PR;SD;PD"))),
    rHandsontableOutput(ns("cat_table")),
    br(),
    fluidRow(column(12,verticalLayout(actionButton(ns("save_table"), "Save")))),
    hr()
  )
}

server_cat <- function(id, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      cat_values <- reactiveValues()

      cat_table_gen <- reactive({
        create_cat_table(6)
      })

      observe({
        if (!is.null(input$cat_table)) {
          cat_values[["previous_res"]] <- isolate(cat_values[[ns("cat_table")]])
          DF = hot_to_r(input$cat_table)
        } else {
          if (is.null(cat_values[[ns("cat_table")]]))
            DF <- cat_table_gen()
          else
            DF <- cat_values[[ns("cat_table")]]
        }
        cat_values[[ns("cat_table")]] <- DF
      })


      observeEvent(input$save_table,{
        all_outcome <- isolate(app_values[['all_outcome']])
        tab_df <- isolate(cat_values[[ns("cat_table")]])
        app_values[['all_outcome']] <- merge_outcome(all_outcome,tab_df, ns)
      })

      output$cat_table <- renderRHandsontable({

        if (is.null(cat_values[[ns("cat_table")]])){
          res_table  = cat_table_gen()
        }else{
          res_table = cat_values[[ns("cat_table")]]
        }

        sumtable <- isolate(app_values[['summary_table']])
        all_outcome <- isolate(app_values[['all_outcome']])
        tr_sub_list <- make_trsub_list(sumtable, all_outcome)

        res_table <- adjust_row(res_table, input$n_arms)
        res_table <- adjust_col(res_table, input$cat_level_name)

        out_table <- rhandsontable(res_table ,useTypes = TRUE, selectCallback = TRUE, height = 200) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE,  rowHeaderWidth = 0, stretchH = 'all') %>%
          hot_col(col = "ID", readOnly = TRUE) %>%
          hot_col("Treatment",type = "autocomplete", source = tr_sub_list$Treatment, strict = FALSE) %>%
          hot_col("Subgroup", type = "autocomplete", source = tr_sub_list$Subgroup , strict = FALSE)

        out_table
      })
    }
  )
}
#
# library(shiny)
# library(rhandsontable)
# source("R/functions_helper.R")
#
# ui <- fluidPage(
#   ui_cat ("counter1"),
#   hr(),
#     textOutput("text"),
#     dataTableOutput('table')
#   )
#
# server <- function(input, output, session) {
#   values <- reactiveValues()
#   df <- read.csv("ssss.csv")
#   values[['summary_table']] <- df
#   values[["all_outcome"]] <- NULL
#   server_cat("counter1",values)
#   output$text <- renderText({
#     paste0(names(values),sep = "  //")
#   })
#   output$table <- renderDataTable(
#     #browser()
#     values[["all_outcome"]]
#     )
# }
#
# shinyApp(ui, server)
