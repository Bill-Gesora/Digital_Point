# 
# #My libraries
# library(rhandsontable)
# library(dplyr)
# library(odbc)
# library(shinyWidgets)
# library(lubridate)
# library(shinyalert)
# library(bs4Dash)
# library(plotly)
# library(shiny)
# library(shinymanager)
# library(DBI)
# 
# con <- dbConnect(odbc::odbc(), "mysqldb")
# 
# # data.frame with credentials info
# # define some credentials
# credentials <- data.frame(
#   user = dbGetQuery(con,
#                     "SELECT [user] FROM App_Users"), # mandatory
#   password = dbGetQuery(con,
#                         "SELECT password FROM App_Users"),
#   stringsAsFactors = FALSE
# )


#Handsontable----
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
    choices = unique(polling_data$county_name),
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
    choices = unique(polling_data$county_name),
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
    choices = unique(polling_data$county_name),
    selected = NULL,
    options = list(placeholder = 'County',
                   plugins = list('remove_button'))
  )
  
  fluidPage(
    ### This is to adjust the width of pop up "showmodal()" for DT modify table
    tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
    useShinyalert(), # Set up shinyalert
    
    fluidRow(
      column(3,
             prettyRadioButtons(
               ns("tracker_view"),
               label = "Select view",
               choices = c("County View",
                           "Constituency View",
                           "Ward View")
             ),
             verbatimTextOutput(ns("abc"))
            ),
      column(7,
             conditionalPanel(condition = "input.tracker_view == 'Ward View'",
                              ns = ns,
                              fluidRow(
                                column(4, county_selector_t_w),
                                column(4, const_selector_t_w),
                                column(3, caw_selector_t_w),
                                column(1, actionButton(
                                  ns('save_tracker_w'),
                                  label = "Save",
                                  styleclass = "info"
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
                                  styleclass = "info"
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
                                  styleclass = "info"
                                ))
                              )
             ),
             rHandsontableOutput(ns('tracker_table')),
             br(),
             conditionalPanel(condition = "input.tracker_view == 'Ward View'",
                              actionButton(
                                ns('save_tracker_w'),
                                label = "Save",
                                styleclass = "info"
                              )
             ),
             conditionalPanel(condition = "input.tracker_view == 'Constituency View'",
                              actionButton(
                                ns('save_tracker_c'),
                                label = "Save",
                                styleclass = "info"
                              )
             ),
             conditionalPanel(condition = "input.tracker_view == 'County View'",
                              actionButton(
                                ns('save_tracker_co'),
                                label = "Save",
                                styleclass = "info"
                              )
             )
      )
    ),
    br(),
    br(),
    fluidRow(
      plotlyOutput(ns('trend_chart'))
    )
  )
}

trackerServer <- function(input,output,session, user_auth, polling_data_map, polling_data_all_map) {
        useShinyalert()
      
      output$abc <- renderPrint({
        as.character(user_auth())
      })
      
      
      #Filtered updates for the tracker----
      filtered_updates_tracker_w <- observe({
        if(is.not.null(input$county_name_id_t_w) & is.not.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            polling_data_map() %>%
              dplyr::filter(county_name %in% input$county_name_id_t_w) %>%
              dplyr::filter(constituency %in% input$const_name_id_t_w)
          })
          
          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)
          
        } else if (!is.null(input$county_name_id_t_w) & is.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            polling_data_map() %>%
              dplyr::filter(county_name %in% input$county_name_id_t_w)
          })
          
          updateSelectizeInput(session, "const_name_id_t_w", choices = dt()$constituency, selected = NULL)
          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)
          
        }
      })
      
      filtered_updates_tracker_c <- observe({
        if (!is.null(input$county_name_id_t_c) & is.null(input$const_name_id_t_c)){
          dt <- reactive({
            polling_data_map() %>%
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
              stats_data3 <- union(inner_join(polling_data_map()[,-c(14,15)],
                                              stats_data2[,c(1,3)],
                                              by = c('county_name'='County')),
                                     polling_data_map()[,-15] %>%
                                     filter(county_name %notin% stats_data2$County)
              ) %>% 
                mutate(As_at = Sys.Date())
              
              
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                #dbRemoveTable(con, "Polling_Data")
                
                #deleting existing data first
                
                dbExecute(con,
                          "DELETE FROM Polling_Data
                           WHERE CAST(As_at AS DATE) = ?
                           AND [User] = ?",
                          params = list(Sys.Date(),as.character(user_auth())) 
                )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)
                
                shinyalert(title = "Saved!", type = "success")
              }
              
            }
          }, ignoreInit = TRUE)
          
          stats_data <- polling_data_map() %>%
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
              rowHeaders = NULL) %>%
              hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                      highlightCol = TRUE, columnSorting = TRUE,
                      colWidths = 100) %>%
              hot_col(col = 'County', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 200) %>%
              hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 100)
          )
          
          trend_data <- polling_data_all_map() %>% 
            mutate(Period = month(As_at, label = T)) %>% 
            group_by(county_name, Period, Status) %>% 
            summarise(Voters = sum(total_voters))
          
          output$trend_chart <- renderPlotly(
            if(!is.null(input$county_name_id_t_co) & !is.not.null(input$const_name_id_t_co)){
              ggplotly( 
                ggplot(trend_data %>% 
                         filter(county_name %in% input$county_name_id_t_co),
                       aes(fill = Status, y = Voters, x = Period, text = paste('County:',county_name,
                                                                               '<br> Voters:',format(Voters,
                                                                                                     format = "f",
                                                                                                     big.mark = ",",
                                                                                                     nsmall =0)
                       )
                       )
                ) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            } else {
              ggplotly( 
                ggplot(trend_data,
                       aes(fill = Status, y = Voters, x = Period, text = paste('County:',county_name,
                                                                               '<br> Voters:',format(Voters,
                                                                                                     format = "f",
                                                                                                     big.mark = ",",
                                                                                                     nsmall =0)))) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            }
          )
          
          
          
        } else if(input$tracker_view == 'Constituency View') {
          filtered_updates_tracker_c
          observeEvent(input$save_tracker_c,{
            tracker_table = isolate(input$tracker_table)
            if (!is.null(tracker_table)) {
              stats_data2 <- hot_to_r(input$tracker_table)
              stats_data3 <- union(inner_join(polling_data_map()[,-c(14,15)],
                                              stats_data2[,c(1,3)],
                                              by = c('constituency'='Constituency')
              ),
              polling_data_map()[,-15] %>%
                filter(constituency %notin% stats_data2$Constituency)
              ) %>% 
                mutate(As_at = Sys.Date())
              
              
              
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                
                #deleting existing data first
                dbExecute(con,
                          "DELETE FROM Polling_Data
                           WHERE CAST(As_at AS DATE) = ?
                           AND [User] = ?",
                          params = list(Sys.Date(),as.character(user_auth())) 
                )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)
                
                shinyalert(title = "Saved!", type = "success")
              }
              
            }
          }, ignoreInit = TRUE)
          
          stats_data <- polling_data_map() %>%
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
              rowHeaders = NULL) %>%
              hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                      highlightCol = TRUE, columnSorting = TRUE,
                      colWidths = 100) %>%
              hot_col(col = 'Constituency', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 200) %>%
              hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 100)
          )
          
          trend_data <- polling_data_all_map() %>% 
            mutate(Period = month(As_at, label = T)) %>% 
            group_by(constituency, Period, Status) %>% 
            summarise(Voters = sum(total_voters)
            )
          
          output$trend_chart <- renderPlotly(
            if(!is.null(input$const_name_id_t_c)){
              ggplotly( 
                ggplot(trend_data %>% 
                         filter(constituency %in% input$const_name_id_t_c),
                       aes(fill = Status, y = Voters, x = Period, text = paste('Constituency:',constituency,
                                                                               '<br> Voters:',Voters))
                ) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            } else {
              ggplotly( 
                ggplot(trend_data,
                       aes(fill = Status, y = Voters, x = Period, text = paste('Constituency:',constituency,
                                                                               '<br> Voters:',Voters))) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            }
          )
        } else if(input$tracker_view == 'Ward View'){
          filtered_updates_tracker_w
          observeEvent(input$save_tracker_w,{
            tracker_table = isolate(input$tracker_table)
            if (!is.null(tracker_table)) {
              stats_data2 <- hot_to_r(input$tracker_table)
              stats_data3 <- union(inner_join(polling_data_map()[,-c(14,15)],
                                              stats_data2[,c(1,3)],
                                              by = c('caw_name'='Ward')
              ),
              polling_data_map()[,-15] %>%
                filter(caw_name %notin% stats_data2$Ward)
              ) %>% 
                mutate(As_at = Sys.Date())
              
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                #dbRemoveTable(con, "Polling_Data")
                
                #deleting existing data first
                dbExecute(con,
                          "DELETE FROM Polling_Data
                           WHERE CAST(As_at AS DATE) = ?
                           AND [User] = ?",
                          params = list(Sys.Date(),as.character(user_auth())) 
                )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)
                
                shinyalert(title = "Saved!", type = "success")
              }
              
            }
          }, ignoreInit = TRUE)
          
          stats_data <- polling_data_map() %>%
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
              rowHeaders = NULL) %>%
              hot_col(col = 'Status', type = 'dropdown', source = c('Won','Lost','Unsure'),
                      highlightCol = TRUE, columnSorting = TRUE,
                      colWidths = 100) %>%
              hot_col(col = 'Ward', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 200) %>%
              hot_col(col = 'Voters', readOnly = TRUE, columnSorting = TRUE,
                      colWidths = 100)
          )
          
          trend_data <- polling_data_all_map() %>% 
            mutate(Period = month(As_at, label = T)) %>% 
            group_by(caw_name, Period, Status) %>% 
            summarise(Voters = sum(total_voters))
          
          
          output$trend_chart <- renderPlotly(
            if(!is.null(input$caw_name_id_t_w)){
              ggplotly( 
                ggplot(trend_data %>% 
                         filter(caw_name %in% input$caw_name_id_t_w ),
                       aes(fill = Status, y = Voters, x = Period, text = paste('Ward:',caw_name,
                                                                               '<br> Voters:',format(Voters,
                                                                                                     format = "f",
                                                                                                     big.mark = ",",
                                                                                                     nsmall =0)))
                ) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            } else {
              ggplotly( 
                ggplot(trend_data,
                       aes(fill = Status, y = Voters, x = Period, text = paste('Ward:',caw_name,
                                                                               '<br> Voters:',format(Voters,
                                                                                                     format = "f",
                                                                                                     big.mark = ",",
                                                                                                     nsmall =0)))) +
                  geom_bar(position = "dodge", stat = 'identity') +
                  theme(axis.title = element_blank(),
                        legend.title = element_blank()
                  ) +
                  scale_fill_manual(values = c("#FF756B","#D1D3D4","#76FEC5")),
                tooltip = c("text")
              ) %>% config(displayModeBar = F)
            }
          )
          
        }
        
      })
    }


##Data upload module----
dataUploadUI <- function(id, label = "whatsapp module") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3,br(),
             div(
               wellPanel(
                 div(img(src = "https://image.flaticon.com/icons/svg/733/733585.svg",
                         width = '70px', height = '70px', align = 'center'),
                     style = "padding-left:30px;"),
                 tags$br(),
                 tags$h6("Enter the title of group", style = "font-weight:bold;"),
                 br(),
                 textInput("group_name", NULL, NULL),
                 hr(),
                 br(),
                 tags$h6("Select the whatsapp file", style = "font-weight:bold;"),
                 actionButton(ns("Btn_GetWhatsapp"),
                              'Choose a file',
                              styleclass = "primary"),
                 hr(),
                 br(),
                 tags$h6('Analyze', style = "font-weight:bold;"),
                 actionButton(ns('whatsapp_btn'),
                              'Analyze',
                              styleclass = "primary"),
                 hr()
               )
               , style = "padding-left:10px; padding-top:10px;")
      ),
      column(9,
             fluidRow(
               column(6,
                      bs4Card(
                        id =ns("Card1"),
                        maximizable = TRUE,
                        closable = FALSE,
                        title = NULL,
                        status = 'info',
                        plotOutput(ns("wa_wordcloud")),
                        headerBorder = FALSE
                      )
               ),
               column(6,
                      bs4Card(
                        id = ns("Card2"),
                        maximizable = TRUE,
                        closable = FALSE,
                        title = NULL,
                        status = 'info',
                        plotlyOutput(ns("wa_sentiment_chart"),
                                     width = '390px',
                                     height = '230px'),
                        headerBorder = FALSE
                      )
               )
             ),
             fluidRow(
               column(6,div(
                 bs4Card(
                   id = ns("Card3"),
                   maximizable = TRUE,
                   closable = FALSE,
                   title = NULL,
                   status = 'info',
                   plotlyOutput(ns("wa_authors_chart")),
                   headerBorder = FALSE
                 ),style = "padding:0;")
               ),
               column(6,div(
                 bs4Card(
                   id = ns("Card4"),
                   maximizable = TRUE,
                   closable = FALSE,
                   title = NULL,
                   status = 'info',
                   echarts4rOutput(ns("wa_time_chart")),
                   headerBorder = FALSE
                 ),style = "padding:0;")
               )
             )
      )
    )
  )
}

dataUploadServer <- function(id){
  moduleServer(
    id,
    
    function(input,output,session) {
      
      observeEvent(input$Btn_GetWhatsapp,{
        path <- file.choose()
        
        progressSweetAlert(
          session = session 
          ,id = "myprogress",
          title = "Upload in progress...",
          display_pct = F, value = 100
        )
        
        whatsappData <- rwa_read(path)
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title =" Upload completed !",
          type = "success"
        )
        
        # output$abc <- renderTable({
        #   head(unlist(whatsappData))
        # })
        observeEvent(input$whatsapp_btn, {
          progressSweetAlert(
            session = session 
            ,id = "myprogress",
            title = HTML(paste0("Analysis in progress...",
                                br(),
                                "This may take upto 5 mins")
            ),
            display_pct = F, value = 100
          )
          
          stop_words <- rbind(
            stop_words,
            data.frame(word = c('omit','deleted','media',
                                'delete','ommitted','omitted',
                                'ni','na','ya',
                                'za','hii','ndio',
                                'kwa','haha','lakini',
                                'bado','hehe','ha',
                                'hahaha'),
                       lexicon = c(rep('SMART',19))
            )
          )
          
          whatsapp_tidy <- whatsappData %>% 
            unnest_tokens(word,text) %>% 
            anti_join(stop_words) %>% 
            select(word) %>% 
            count(word, sort = TRUE) %>% 
            mutate(freq = n)
          names(whatsapp_tidy)
          
          output$wa_wordcloud <- renderPlot({ 
            wordcloud(words = whatsapp_tidy$word, freq = whatsapp_tidy$freq, min.freq = 1,
                      max.words=100, random.order=FALSE, rot.per=0.2,
                      colors=brewer.pal(8, "Dark2"))
          })
          
          #Creating chart for multiple users----
          authors_data <- whatsappData %>% 
            group_by(author) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            head(10)
          
          
          #Plotting my graph
          output$wa_authors_chart <- renderPlotly({
            authors_data %>% 
              plot_ly(labels = ~author, values = ~count,
                      textposition = 'outside',
                      margin = list(
                        l = -10,
                        r = 0,
                        b = 0,
                        t = 0
                      ),
                      textinfo = ~author,
                      autosize = F, width = 370, height = 220,
                      hoverinfo = 'skip',
                      textfont = list(size = 5),
                      text = ~author) %>% 
              add_pie(hole = 0.7) %>% 
              layout(title = NULL,  showlegend = F,
                     uniformtext=list(minsize=8, mode='hide'),
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
              config(displayModeBar = F)
          })
          
          output$wa_time_chart <- renderEcharts4r({
            whatsappData$hours <- hour(whatsappData$time)
            
            whatsappData %>% 
              group_by(hours) %>% 
              summarise(count = n()) %>% 
              ungroup() %>% 
              e_charts(hours) %>% 
              e_polar() %>% 
              e_angle_axis(hours) %>% # angle = x
              e_radius_axis(show = FALSE) %>% 
              e_area(count, coord_system = "polar",
                     symbol = "none") %>% 
              e_legend(show = FALSE)
            
          })
          
          #Creating chart for sentiments----
          bing_word_counts2 <- whatsapp_tidy %>%
            inner_join(get_sentiments("bing")) %>%
            ungroup()
          
          bing_word_counts2 %>%
            group_by(sentiment) %>%
            top_n(n = 10, wt = freq) %>%
            ungroup() %>%
            # mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, freq, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = NULL,
                 x = NULL) +
            coord_flip() +
            theme(axis.text.x = element_blank(),
                  axis.ticks = element_blank()) 
          
          output$wa_sentiment_chart <- renderPlotly({ 
            ggplotly(height = 250,
                     bing_word_counts2 %>%
                       group_by(sentiment) %>%
                       top_n(n = 15, wt = freq) %>%
                       ungroup() %>%
                       mutate(word = reorder(word, freq)) %>%
                       ggplot(aes(word, freq, fill = sentiment)) +
                       geom_col(show.legend = FALSE) +
                       coord_flip() +
                       labs(y = NULL,
                            x = NULL) +
                       facet_wrap(~sentiment, scales = "free_y") +
                       theme(axis.text.x = element_blank(),
                             axis.ticks = element_blank(),
                             panel.spacing.x=unit(0.2, "lines"),
                             legend.position = 'none')
            ) %>% config(displayModeBar = F)
          })
          
          closeSweetAlert(session = session)
          sendSweetAlert(
            session = session,
            title =" Analysis completed !",
            type = "success"
          )
          
        })
        
        
      })
    }
  )
}


#Application side-------

# ui <- bs4DashPage(
#   #Authentication
#   #Authentication
#   auth_ui(
#     id = "auth",
#     # add image on top ?
#     tags_top =
#       tags$div(
#         tags$h4("Digital Point", style = "align:center"),
#         tags$img(
#           src = "https://t3.ftcdn.net/jpg/03/49/50/42/240_F_349504264_NvpEyjtwOragV7gqAatO6iDj6BPw1tqE.jpg", width = 100
#         )
#       ),
#     # add information on bottom ?
#     tags_bottom = tags$div(
#       tags$p(
#         "For any question, please  contact ",
#         tags$a(
#           href = "mailto:gesorabill@gmail.com?Subject=Shiny%20aManager",
#           target="_top", "administrator"
#         )
#       )
#     ),
#     # change auth ui background ?
#     background  = "linear-gradient(rgba(0, 0, 255, 0.5),
#                        rgba(255, 255, 0, 0.5)),
#                        url('data:image/jpeg;base64,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');"
#   ),
#   trackerUI("tracker_module")
# )
# 
# server <- function(input, output, session) {
# 
#   #Authentication of the shinymanager package
#   auth <- callModule(
#     module = auth_server,
#     id = "auth",
#     check_credentials = check_credentials(credentials)
#   )
# 
#   #defining my rective datasets
#   user_auth <- reactive(auth$user)
#   polling_data_map <- reactive({
#                       mapping_data <- dbSendQuery(con,"SELECT *
#                                                        FROM polling_data_recent
#                                                        WHERE [User] = ?")
#                       dbBind(mapping_data,as.character(list(auth$user)))
#                       dbFetch(mapping_data)
#                     })
# 
#   polling_data_all_map <- reactive({
#                       mapping_data_all <- dbSendQuery(con,"SELECT *
#                                                       FROM polling_data
#                                                      WHERE [User] = ?")
#                                      dbBind(mapping_data_all,as.character(list(auth$user)))
#                                      dbFetch(mapping_data_all)
#                                        })
# 
#  
#   
#   callModule(trackerServer,"tracker_module",user_auth, polling_data_map, polling_data_all_map)
# 
# }
# 
# shinyApp(ui,server)












