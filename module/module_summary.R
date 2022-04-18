
ui_summary <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      fluid = FALSE,

      # Define the sidebar with one input
      sidebarPanel(width = 3,
                   h4(strong("Total Trials Summary:")),
                  # fluidRow(style = "height:150px;",plotlyOutput(ns("p_year"))),
                   fluidRow(style = "height:180px;",plotlyOutput(ns("p_outcome"))),
                   fluidRow(style = "height:180px;",plotlyOutput(ns("p_treatment"))),
                   fluidRow(plotlyOutput(ns("p_subgroup")))
      ),
      mainPanel(
        width = 9,
        #h4(strong("Data Table:")),
        fluidRow(
          tabsetPanel(
          id = "dt_table",
          type = "tabs",
          tabPanel("Total Trials",
                   #h3(" "),
                   DT::DTOutput(ns('tbl')),
                   checkboxInput(ns("dt_sel"), "Select All"),
          ),
          tabPanel("Selected Trials",
                   h3(" "),
                   DT::DTOutput(ns('tbl_sel'))
          )
        )),
        fluidRow(
        #h5("  "),
        h5(strong("Selected Trials Summary:"))),
        fluidRow(
          column(4,
            rHandsontableOutput(ns("sel_sum_table"))

        ),
        column(2,
               fluidRow(style = "height:150px;",plotlyOutput(ns("plot_treatment")))
        ),
        column(2,
               fluidRow(style = "height:150px;",plotlyOutput(ns("plot_Subgroup")))
        ),
        column(2,
               fluidRow(style = "height:150px;",plotlyOutput(ns("plot_Phase")))
        ),
        column(2,
               fluidRow(plotlyOutput(ns("plot_TrLine")))
        ))

      )
    ))
}

server_summary <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      values[['Sel_Table']]  <- NULL

      m <- list(
        l = 30,
        r = 10,
        b = 0,
        t = 30,
        pad = 0
      )
      plot_heights <- 180

      ### studies statistics
      output$p_year <- renderPlotly({
        if(!is.null(values[['rds']])){
        df <- values[['rds']]
        heightss <- plot_heights #length(unique(df$Year)) *60
        p1 <- ggplot(data=df, aes(Year)) +
          geom_bar(stat = "count", fill="white", color="gray") + theme_classic() #+ coord_flip()
        ggplotly(p1) %>% layout(height = heightss, margin = m)
        }
      })

      output$p_outcome <- renderPlotly({
        if(!is.null(values[['rds']])){
        ddf <- values[['rds']]
        heightss <- plot_heights  #length(unique(df$Type)) *60
        Outcome <- unlist(strsplit(ddf$Outcome,";"))

        p1 <- ggplot(data=as.data.frame(Outcome), aes(Outcome)) +
          geom_bar(stat = "count", fill="white", color="gray") + theme_classic()# + coord_flip()
        ggplotly(p1) %>% layout(height = heightss,margin = m)
        }
      })

      output$p_treatment <- renderPlotly({
        if(!is.null(values[['rds']])){
        df <- values[['rds']]
        heightss <- plot_heights  #length(unique(df$Treatment)) *60
        p1 <- ggplot(data=df, aes(Treatment)) +
          geom_bar(stat = "count", fill="white", color="gray") + theme_classic() #+ coord_flip()
        ggplotly(p1) %>% layout(height = heightss,margin = m)
        }
      })

      output$p_subgroup <- renderPlotly({
        if(!is.null(values[['rds']])){
        df <- values[['rds']]
        heightss <- plot_heights  #length(unique(df$Subgroup)) *60
        p1 <- ggplot(data=df, aes(Subgroup)) +
          geom_bar(stat = "count", fill="white", color="gray") + theme_classic() #+ coord_flip()
        ggplotly(p1) %>% layout(height = heightss,margin = m)
        }
      })

      output$tbl <- DT::renderDT({
        if(!is.null(values[['rds']])){
        ddf <- values[['rds']]
        col_show <- c("NCT","Year","Type", "Phase", "TrLine","Treatment", "Subgroup", "Outcome")
        ptable<- ddf[,col_show] %>% mutate_at(col_show, factor)

        DT::datatable(ptable,
                      rownames = FALSE,
                      filter = 'top',
                      options = list(
                        dom = 'ft',
                        ordering=F,
                        scrollY = 230,
                        scrollX = TRUE,
                        paging = FALSE)
        )}
      },
      server = TRUE
      )



      tbl_proxy <- DT::dataTableProxy("tbl")

      observeEvent(input$dt_sel, {
        if (isTRUE(input$dt_sel)) {
          DT::selectRows(tbl_proxy, input$tbl_rows_all)
        } else {
          DT::selectRows(tbl_proxy, NULL)
        }
      })

      observe({
        selected <- input$tbl_rows_selected
        ddf <- values[['rds']]
        values[['Sel_Table']] <- ddf[selected,]
      })

      output$tbl_sel <- DT::renderDataTable({
        if(!is.null(values[['Sel_Table']])){
        ddf <- values[['Sel_Table']]
        col_show <- c("NCT","Year","Type", "Phase", "TrLine","Treatment", "Subgroup", "Outcome")
        ptable<- ddf[,col_show] %>% mutate_at(col_show, factor)



        DT::datatable(ptable,
                      rownames = FALSE,
                      options = list(
                        dom = 'ft',
                        ordering=T,
                        scrollY = 300,
                        scrollX = TRUE,
                        paging = FALSE)
        )
        }
      }
      )

      output$sel_sum_table <- renderRHandsontable({
        if(!is.null(values[['Sel_Table']])){
        ddf <- values[['Sel_Table']]

        #browser()

        if(nrow(ddf) == 0){
        sel_sum_t <- data.frame(
          'x1' = c("No.Study","No.Trials","Treatment","Subgroup","Type","Phase","TrLine","Outcome"),
          'x2' = c("0","0",NA,NA,NA,NA,NA,NA)
        )}else{
          sel_sum_t <- data.frame(
            'x1' = c("No.Study","No.Trials","Treatment","Subgroup","Type","Phase","TrLine","Outcome"),
            'x2' =         c(as.character(length(unique(ddf$NCT))),
                             as.character(nrow(ddf)),
                             paste0(unique(ddf$Treatment),collapse = "; "),
                             paste0(unique(ddf$Subgroup),collapse = "; "),
                             paste0(unique(ddf$Type),collapse = "; "),
                             paste0(unique(ddf$Phase),collapse = "; "),
                             paste0(unique(ddf$TrLine),collapse = "; "),
                             ddf$Outcome[which.max(nchar(ddf$Outcome))])
          )
        }

        ## output table
        out_table <- rhandsontable(sel_sum_t ,useTypes = TRUE, selectCallback = TRUE, rowHeaders = NULL,colHeaders = NULL,height = 250)
        out_table
        }
      })

      output$plot_treatment <-renderPlotly({
        if(!is.null(values[['Sel_Table']])){
        ddf <- values[['Sel_Table']]
        if(nrow(ddf)> 0){
          p1 <-make_pie(ddf$Treatment,"Treatment")
          p1
        }
        }
      })

      output$plot_Subgroup <-renderPlotly({
        if(!is.null(values[['Sel_Table']])){
          ddf <- values[['Sel_Table']]
        if(nrow(ddf)> 0){
          p1 <-make_pie(ddf$Subgroup,"Subgroup")
          p1
        }
        }
      })

      output$plot_Phase <-renderPlotly({
        if(!is.null(values[['Sel_Table']])){
        ddf <- values[['Sel_Table']]
        if(nrow(ddf)> 0){
          p1 <-make_pie(ddf$Phase,"Phase")
          p1
        }
        }
      })

      output$plot_TrLine <-renderPlotly({
        if(!is.null(values[['Sel_Table']])){
        ddf <- values[['Sel_Table']]
        if(nrow(ddf)> 0){
          p1 <-make_pie(ddf$TrLine,"TrLine")
          p1
        }
        }
      })


    }
  )
}

# library(shiny)
# library(plotly)
# source("functions_helper.R")


# ui <- fluidPage(
#   ui_summary ("ss")
# )
#
# server <- function(input, output, session) {
#   values <- reactiveValues()
#   ddf <- readRDS("C:/Users/Preadmin/OneDrive - Telperian/MetaToolApp/Source.RDS")
#   outcome_names_all <- stringr::str_remove(colnames(ddf)[grepl('\\b.N\\b',colnames(ddf))], ".N")
#   outcome_matrix <- !is.na(ddf[colnames(ddf)[grepl('.N\\b',colnames(ddf))]])
#   ddf$Outcome <- apply(outcome_matrix, 1, function(x) paste0(outcome_names_all[x],collapse = '; '))
#   values[['SumTable']] <- ddf
#   server_summary("ss",values)
# }
#
# shinyApp(ui, server)
