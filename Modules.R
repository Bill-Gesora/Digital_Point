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
           )),
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

trackerServer <- function(id){
  useShinyalert()
  moduleServer(
    id,

    function(input,output,session) {

      #Filtered updates for the tracker----
      filtered_updates_tracker_w <- observe({
        if(is.not.null(input$county_name_id_t_w) & is.not.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            polling_data %>%
              dplyr::filter(county_name %in% input$county_name_id_t_w) %>%
              dplyr::filter(constituency %in% input$const_name_id_t_w)
          })

          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)

        } else if (!is.null(input$county_name_id_t_w) & is.null(input$const_name_id_t_w) & is.null(input$caw_name_id_t_w)){
          dt <- reactive({
            polling_data %>%
              dplyr::filter(county_name %in% input$county_name_id_t_w)
          })

          updateSelectizeInput(session, "const_name_id_t_w", choices = dt()$constituency, selected = NULL)
          updateSelectizeInput(session, "caw_name_id_t_w", choices = dt()$caw_name, selected = NULL)

        }
      })

      filtered_updates_tracker_c <- observe({
        if (!is.null(input$county_name_id_t_c) & is.null(input$const_name_id_t_c)){
          dt <- reactive({
            polling_data %>%
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
              stats_data3 <- union(inner_join(polling_data[,-c(14,15)],
                                       stats_data2[,c(1,3)],
                                       by = c('county_name'='County')),
                                   polling_data[,-15] %>%
                                     filter(county_name %notin% stats_data2$County)
                                  ) %>% 
                             mutate(As_at = Sys.Date())
                              

              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                #dbRemoveTable(con, "Polling_Data")
                
                #deleting existing data first
                dbExecute(con,
                          "DELETE FROM Polling_Data
                         WHERE CAST(As_at AS DATE) = ?",
                          params = Sys.Date()
                )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)

                shinyalert(title = "Saved!", type = "success")
              }

            }
          }, ignoreInit = TRUE)

          stats_data <- polling_data %>%
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
          
          trend_data <- polling_data_all %>% 
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
              stats_data3 <- union(inner_join(polling_data[,-c(14,15)],
                                              stats_data2[,c(1,3)],
                                              by = c('constituency'='Constituency')
                                              ),
                                     polling_data[,-15] %>%
                                     filter(constituency %notin% stats_data2$Constituency)
                                    ) %>% 
                mutate(As_at = Sys.Date())
              
              
              
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){

                #deleting existing data first
                dbExecute(con,
                          "DELETE FROM Polling_Data
                         WHERE CAST(As_at AS DATE) = ?",
                          params = Sys.Date()
                )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)

                shinyalert(title = "Saved!", type = "success")
              }

            }
          }, ignoreInit = TRUE)

          stats_data <- polling_data %>%
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
          
          trend_data <- polling_data_all %>% 
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
              stats_data3 <- union(inner_join(polling_data[,-c(14,15)],
                                              stats_data2[,c(1,3)],
                                              by = c('caw_name'='Ward')
                                              ),
                                    polling_data[,-15] %>%
                                    filter(caw_name %notin% stats_data2$Ward)
                                  ) %>% 
                mutate(As_at = Sys.Date())
              
              # Drop table if it already exists
              if (dbExistsTable(con, "Polling_Data")){
                #dbRemoveTable(con, "Polling_Data")
                
                #deleting existing data first
                dbExecute(con,
                        "DELETE FROM Polling_Data
                         WHERE CAST(As_at AS DATE) = ?",
                         params = Sys.Date()
                        )
                
                # Write the data frame to the database
                dbAppendTable(con, name = "Polling_Data", value = stats_data3)

                shinyalert(title = "Saved!", type = "success")
              }

            }
          }, ignoreInit = TRUE)

          stats_data <- polling_data %>%
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
          
          trend_data <- polling_data_all %>% 
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
  )
}


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

