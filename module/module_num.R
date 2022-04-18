
ui_num <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
     column(6,
            numericInput(ns("n_arms"), label = h5(strong("Number of Cohorts:")), 7, min = 1, max = 100))),
     rHandsontableOutput(ns("num_table")),
    br(),
    fluidRow(column(12,verticalLayout(actionButton(ns("save_table"), "Save")))),
    hr()
  )
}

server_num <- function(id, app_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      con_values <- reactiveValues()

      num_table_gen <- reactive({
        create_num_table(6)
      })

      observe({
        if (!is.null(input$num_table)) {
          con_values[["previous_res"]] <- isolate(con_values[[ns("num_table")]])
          DF = hot_to_r(input$num_table)
        } else {
          if (is.null(con_values[[ns("num_table")]]))
            DF <- num_table_gen()
          else
            DF <- con_values[[ns("num_table")]]
        }
        con_values[[ns("num_table")]] <- DF
      })

      observeEvent(input$save_table,{
        all_outcome <- isolate(app_values[['all_outcome']])
        tab_df <- isolate(con_values[[ns("num_table")]])
        app_values[['all_outcome']] <- merge_outcome(all_outcome,tab_df, ns)
      })

      output$num_table <- renderRHandsontable({

        if (is.null(con_values[[ns("num_table")]])){
          res_table  = num_table_gen()
        }else{
          res_table = con_values[[ns("num_table")]]
        }

        sumtable <- isolate(app_values[['summary_table']])
        all_outcome <- isolate(app_values[['all_outcome']])
        tr_sub_list <- make_trsub_list(sumtable, all_outcome)

        res_table <- adjust_row(res_table, input$n_arms)

        out_table <- rhandsontable(res_table ,useTypes = TRUE, selectCallback = TRUE, height = 200) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE,  rowHeaderWidth = 0, stretchH = 'all') %>%
          hot_col(col = "ID", readOnly = TRUE) %>%
          hot_col("Treatment",type = "autocomplete", source = tr_sub_list$Treatment , strict = FALSE) %>%
          hot_col("Subgroup",type = "autocomplete", source = tr_sub_list$Subgroup , strict = FALSE)

        out_table
      })
    }
  )
}

# library(shiny)
# library(rhandsontable)
# source("R/functions_helper.R")
#
# ui <- fluidPage(
#   ui_num("counter1"),
#   hr(),
#   textOutput("text"),
#   dataTableOutput('table')
# )
#
# server <- function(input, output, session) {
#   values <- reactiveValues()
#   df <- read.csv("ssss.csv")
#   values[['summary_table']] <- df
#   server_num("counter1",values)
#   output$text <- renderText({
#     paste0(names(values),sep = "  //")
#     })
#   output$table <- renderDataTable(
#     #browser()
#     values[["all_outcome"]]
#   )
#
# }
#
# shinyApp(ui, server)
