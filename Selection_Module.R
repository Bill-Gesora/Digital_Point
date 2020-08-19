library(shiny)
library(shinyWidgets)
library(rhandsontable)
library(dplyr)
library(shinyWidgets)
library(shinyalert)
library(odbc)
library(DBI)
library(echarts4r)

con <- dbConnect(odbc::odbc(), "mysqldb")
stats_data_raw <- dbGetQuery(con,
                             "SELECT * FROM Digital_Point.dbo.Polling_Data")

is.not.null <- function(x) !is.null(x)

#Handsontable module----
#ModuleUI function
trackerUI <- function(id, label = "tracker module") {
  useShinyalert()
  ns <- NS(id)
  
#Filters for each tab----  
  #Ward view
  county_selector_t_w <- selectizeInput(
    ns("county_name_id_t_w"),
    label = "County", 
    multiple = TRUE,
    choices = unique(stats_data_raw$county_name),
    selected = NULL,
    options = list(placeholder = 'County',
                   plugins = list('remove_button'))
  )
  
  #constituency_name_selector
  const_selector_t_w <- selectizeInput(
    ns("const_name_id_t_w"),
    label = "Constituency", 
    multiple = TRUE,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = 'Constituency',
                   plugins = list('remove_button'))
  )
  
  #caw_name_selector
  caw_selector_t_w <- selectizeInput(
    ns("caw_name_id_t_w"),
    label = "Ward", 
    multiple = TRUE,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = 'Ward',
                   plugins = list('remove_button'))
  )
  
  #Constituency view
  county_selector_t_c <- selectizeInput(
    ns("county_name_id_t_c"),
    label = "County", 
    multiple = TRUE,
    choices = unique(stats_data_raw$county_name),
    selected = NULL,
    options = list(placeholder = 'County',
                   plugins = list('remove_button'))
  )
  
  #constituency_name_selector
  const_selector_t_c <- selectizeInput(
    ns("const_name_id_t_c"),
    label = "Constituency", 
    multiple = TRUE,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = 'Constituency',
                   plugins = list('remove_button'))
  )
  
  #County view
  county_selector_t_co <- selectizeInput(
    ns("county_name_id_t_co"),
    label = "County", 
    multiple = TRUE,
    choices = unique(stats_data_raw$county_name),
    selected = NULL,
    options = list(placeholder = 'County',
                   plugins = list('remove_button'))
  )
   
  fluidRow(
    ### This is to adjust the width of pop up "showmodal()" for DT modify table 
    tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
    helpText("Note: Remember to save any updates!"),
    br(),
    ### tags$head() is to customize the download button
    useShinyalert(), # Set up shinyalert
    
    column(3,
           prettyRadioButtons(
            ns("tracker_view"),
            label = "Select view", 
            choices = c("County View", 
                        "Constituency View", 
                        "Ward View")
    )),
    column(8,
           conditionalPanel(condition = "input.tracker_view == 'Ward View'", 
                            ns = ns,
                            fluidRow(
                               column(4, county_selector_t_w),
                               column(4, const_selector_t_w),
                               column(3, caw_selector_t_w),
                               column(1, actionButton(
                                 ns('save_tracker_w'),
                                 label = "Save",
                               ))
                              )
                            ),
           conditionalPanel(condition = "input.tracker_view == 'Constituency View'", 
                            ns = ns,
                             fluidRow(
                               column(6, county_selector_t_c),
                               column(5, const_selector_t_c),
                               column(1, actionButton(
                                 ns('save_tracker_c'),
                                 label = "Save",
                               ))
                               )
                            ),
           conditionalPanel(condition = "input.tracker_view == 'County View'",
                            ns = ns,
                             fluidRow(
                               column(11, county_selector_t_co),
                               column(1, actionButton(
                                 ns('save_tracker_co'),
                                 label = "Save",
                               ))
                             )
                           ),
           rHandsontableOutput(ns('tracker_table')),
           br(),
           conditionalPanel(condition = "input.tracker_view == 'Ward View'",
           actionButton(
             ns('save_tracker_w'),
             label = "Save",
             #style = "jelly", 
             #color = "primary"
              )
           ),
           conditionalPanel(condition = "input.tracker_view == 'Constituency View'",
                            actionButton(
                              ns('save_tracker_c'),
                              label = "Save",
                            )
           ),
           conditionalPanel(condition = "input.tracker_view == 'County View'",
                            actionButton(
                              ns('save_tracker_co'),
                              label = "Save",
                              #style = "jelly", 
                              #color = "primary"
                            )
           )
        )
  )
}

trackerServer <- function(id){
  useShinyalert()
  moduleServer(
    id,
    
    function(input,output,session) {
      
      #Filtered updates for the tracker----
      filtered_updates_tracker_w <- observe({
        if(is.not.null(input$county_name_id_t_w) & is.not.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            stats_data_raw %>% 
              dplyr::filter(county_name %in% input$county_name_id_t_w) %>% 
              dplyr::filter(constituency %in% input$const_name_id_t_w)
          })
  
          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)
          
        } else if (!is.null(input$county_name_id_t_w) & is.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            stats_data_raw %>% 
              dplyr::filter(county_name %in% input$county_name_id_t_w)
          })
          
          updateSelectizeInput(session, "const_name_id_t_w", choices = dt()$constituency, selected = NULL)
          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)
          
        }
      })
      
      filtered_updates_tracker_c <- observe({
        if (!is.null(input$county_name_id_t_c) & is.null(input$const_name_id_t_c)){
          dt <- reactive({
            stats_data_raw %>% 
              dplyr::filter(county_name %in% input$county_name_id_t_c)
          })
          
          updateSelectizeInput(session, "const_name_id_t_c", choices = dt()$constituency, selected = NULL)
        }
      })
      
      
      observe({
        input$tracker_view
        
        if(input$tracker_view == 'County View'){
          
          observeEvent(input$save_tracker_co,{
            tracker_table = isolate(input$tracker_table)
            if (!is.null(tracker_table)) {
              stats_data2 <- hot_to_r(input$tracker_table)
              stats_data3 <- left_join(stats_data_raw[,-14],
                                       stats_data2[,c(1,3)], 
                                       by = c('county_name'='County')
                                      )
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                dbRemoveTable(con, "Polling_Data")
              
              shinyalert(title = "Saved!", type = "success")
              }
              
              # Write the data frame to the database
              dbWriteTable(con, name = "Polling_Data", value = stats_data3, row.names = FALSE)
            }
          }, ignoreInit = TRUE)
          
          stats_data <- stats_data_raw %>% 
            group_by(county_name, Status) %>%
            summarise(total_voters = format(round(sum(total_voters)),nsmall=0),
                      polling_stations = n(),
            ) %>% 
            ungroup() %>% 
            transmute(County = county_name,
                      Voters = total_voters,
                      Status)

          output$tracker_table <- renderRHandsontable(
            rhandsontable(
                 stats_data %>% 
                   filter(County %in% input$county_name_id_t_co), 
                 rowHeaders = NULL,
                 height = 350) %>% 
            hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                    highlightCol = TRUE, columnSorting = TRUE,
                    colWidths = 100) %>% 
            hot_col(col = 'County', readOnly = TRUE, columnSorting = TRUE,
                    colWidths = 200) %>% 
            hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                    colWidths = 100)
            )
                    
        } else if(input$tracker_view == 'Constituency View') {
          filtered_updates_tracker_c
          observeEvent(input$save_tracker_c,{
            tracker_table = isolate(input$tracker_table)
            if (!is.null(tracker_table)) {
              stats_data2 <- hot_to_r(input$tracker_table)
              stats_data3 <- left_join(stats_data_raw[,-14],
                                       stats_data2[,c(1,3)], 
                                       by = c('constituency'='Constituency')
              )
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                dbRemoveTable(con, "Polling_Data")
                
                shinyalert(title = "Saved!", type = "success")
              }
              
              # Write the data frame to the database
              dbWriteTable(con, name = "Polling_Data", value = stats_data3, row.names = FALSE)
            }
          }, ignoreInit = TRUE)
          
          stats_data <- stats_data_raw %>% 
            group_by(constituency, Status) %>%
            summarise(total_voters = format(round(sum(total_voters)),nsmall=0),
                      polling_stations = n(),
            ) %>% 
            ungroup() %>% 
            transmute(Constituency = constituency,
                      Voters = total_voters,
                      Status)
          
          output$tracker_table <- renderRHandsontable(
            rhandsontable(
              stats_data %>% 
                filter(Constituency %in% input$const_name_id_t_c), 
              rowHeaders = NULL,
              height = 350) %>% 
              hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                      highlightCol = TRUE, columnSorting = TRUE,
                      colWidths = 100) %>% 
              hot_col(col = 'Constituency', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 200) %>% 
              hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 100)
          )
         } else if(input$tracker_view == 'Ward View'){
           filtered_updates_tracker_w
           observeEvent(input$save_tracker_w,{
             tracker_table = isolate(input$tracker_table)
             if (!is.null(tracker_table)) {
               stats_data2 <- hot_to_r(input$tracker_table)
               stats_data3 <- left_join(stats_data_raw[,-14],
                                        stats_data2[,c(1,3)], 
                                        by = c('caw_name'='Ward')
               )
               # Drop table if it already exists
               if (dbExistsTable(con, "Polling_Data")){
                 dbRemoveTable(con, "Polling_Data")
                 
                 shinyalert(title = "Saved!", type = "success")
               }
               
               # Write the data frame to the database
               dbWriteTable(con, name = "Polling_Data", value = stats_data3, row.names = FALSE)
             }
           }, ignoreInit = TRUE)
           
           stats_data <- stats_data_raw %>% 
             group_by(caw_name, Status) %>%
             summarise(total_voters = format(round(sum(total_voters)),nsmall=0),
                       polling_stations = n(),
             ) %>% 
             ungroup() %>% 
             transmute(Ward = caw_name,
                       Voters = total_voters,
                       Status)
           
           output$tracker_table <- renderRHandsontable(
             rhandsontable(
               stats_data %>% 
                 filter(Ward %in% input$caw_name_id_t_w), 
               rowHeaders = NULL,
               height = 350) %>% 
               hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                       highlightCol = TRUE, columnSorting = TRUE,
                       colWidths = 100) %>% 
               hot_col(col = 'Ward', readOnly = TRUE, columnSorting = TRUE,
                       colWidths = 200) %>% 
               hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                       colWidths = 100)
           )
        }
        
      })
    }
  )
}

#server app----
ui <- fluidPage(
  trackerUI('tracker_table')
)

server <- function(input, output, session) {
  trackerServer('tracker_table')
}

shinyApp(ui, server)