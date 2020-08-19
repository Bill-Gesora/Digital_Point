library(shiny)
library(dplyr)
library(DT)
library(rwhatsapp)
library(shinyFiles)
library(bs4Dash)
library(shinyWidgets)
library(shinyalert)
library(tm)
library(plotly)
library(stringr)
library(wordcloud)
library(tidytext)
library(lubridate)
library(echarts4r)


options(shiny.maxRequestSize = 100*1024^2)
memory.limit(size = 56000)

#Module fileinput----
dataUploadUI <- function(id, label = "whatsapp module") {
  ns <- NS(id)
  fluidRow(
    column(3,br(),
           div(
           wellPanel(
                 div(img(src = "https://image.flaticon.com/icons/svg/733/733585.svg",
                          width = '70px', height = '70px', align = 'center'),
                     style = "padding-left:50px;"),
                 tags$br(),
                 tags$h5("Enter the title of group", style = "font-weight:bold;"),
                 textInput("group_name", NULL, NULL),
                 hr(),
                 br(),
                 tags$h5("Select the whatsapp file", style = "font-weight:bold;"),
                 actionButton(ns("Btn_GetWhatsapp"),
                              'Choose a file'),
                 hr(),
                 br(),
                 tags$h5('Analyze', style = "font-weight:bold;"),
                 actionButton(ns('whatsapp_btn'),
                                 'Analyze'),
                 hr()
                       )
                   , style = "padding-left:20px; padding-top:20px;")
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
                        width = '400px',
                        height = '250px',
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
                  width = '400px',
                  height = '250px',
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
               width = '400px',
               height = '250px',
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
               echarts4rOutput(ns("wa_time_chart"),
                               height = "60%",
                               weight = "60%"),
               width = '400px',
               height = '250px',
               headerBorder = FALSE
             ),style = "padding:0;")
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
          wordcloud(words = whatsapp_tidy$word, freq = a_tidy$freq, min.freq = 1,
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
 
library(shiny)

ui <- bs4DashPage(
  dataUploadUI('upload_module')
)

server <- function(input, output, session) {
  dataUploadServer('upload_module')
}

shinyApp(ui, server)

