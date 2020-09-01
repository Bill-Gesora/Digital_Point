# library(highcharter)
library(shiny)
library(bs4Dash)
library(odbc) # For creating a connection to the database
library(DBI)
library(snakecase)
library(shinyWidgets)
library(htmltools)
library(plotly)
library(ggplot2)
library(DT)
library(htmlwidgets)
library(shinythemes)
library(grillade)
library(leaflet)
library(rhandsontable)
library(shinysky)
library(lubridate)
library(shinyalert)
library(formattable)
library(sf)
library(echarts4r)
library(wordcloud)
library(shinyalert)
library(dplyr)
library(shinymanager)


options(scipen = 999) # turn-off scientific notation like 1e+48
options(shiny.maxRequestSize=10000*1024^2)
options(scipen=999)
options(shiny.sanitize.errors = FALSE)
options(warn=-1) #turning warnings off
options(shiny.maxRequestSize = 100*1024^2)

#Setting options
lang <- shinymanager:::language$new()
lang$add(
  "Please authenticate" = "Sign in"
)



# connection to the db
con <- dbConnect(odbc::odbc(), "mysqldb")

# data.frame with credentials info
# define some credentials
credentials <- data.frame(
  user = dbGetQuery(con,
                    "SELECT [user] FROM App_Users"), # mandatory
  password = dbGetQuery(con,
                        "SELECT password FROM App_Users"),
  stringsAsFactors = FALSE
)

# Importing other R files
source("Data_Manipulation.R")
source("SurveyAnalysis.R")
source("Modules.R")
source("UI_Widgets.R")

# create a custom function
is.not.null <- function(x) !is.null(x)
`%notin%` <- Negate(`%in%`)

# setting my working directory
path <- "C:/Users/bgesora/OneDrive - Deloitte (O365D)/Documents/BG/Personal Projects/R Projects/Digital Point/Workspace/Digital_Point_v1"

ui <- bs4DashPage(
  includeCSS("www/styles.css"),
  #Authentication
  auth_ui(
    id = "auth",
    # add image on top ?
    tags_top =
      tags$div(
        tags$h4("Digital Point", style = "align:center"),
        tags$img(
          src = "https://t3.ftcdn.net/jpg/03/49/50/42/240_F_349504264_NvpEyjtwOragV7gqAatO6iDj6BPw1tqE.jpg", width = 100
        )
      ),
    # add information on bottom ?
    tags_bottom = tags$div(
      tags$p(
        "For any question, please  contact ",
        tags$a(
          href = "mailto:gesorabill@gmail.com?Subject=Shiny%20aManager",
          target="_top", "administrator"
        )
      )
    ),
    # change auth ui background ?
    background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5)),
                       url('data:image/jpeg;base64,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');"
  ),
  enable_preloader = FALSE,
  loading_duration = 5,
  navbar = navbar,
  sidebar = sidebar,
  # controlbar = controlbar,
  # footer = footer,
  title = "test",
  body = body
)
server <- function(input, output, session) {
  source("ServerSide.R", local = TRUE)
  source("Mapping_Data.R", local = TRUE)
  
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  output$res_auth <- renderText({
    auth$user_info[1,1]
  })
  
  user_auth <- reactive(auth$user)
  
  # pollData <- reactivePoll(1000, session,
  #   checkFunc = function(){
  #     query <- dbSendQuery(con,
  #                           "SELECT modify_date
  #                            FROM sys.tables
  #                            WHERE [name] = 'Polling_Data'"
  #                          )
  #     lastFeedback <- dbFetch(query, -1)
  #     dbClearResult(query)
  #     dbDisconnect(con)
  #     
  #     lastFeedback$modify_date
  #   },
  #   valueFunc = function(){
  #     mapping_data <- dbSendQuery(con,"SELECT *
  #                                    FROM polling_data_recent
  #                                    WHERE [User] = ?")
  #     dbBind(mapping_data,as.character(list(auth$user)))
  #     polling_data_map <- dbFetch(mapping_data)
  #     
  #     mapping_data_all <- dbSendQuery(con,"SELECT *
  #                                    FROM polling_data
  #                                    WHERE [User] = ?")
  #     dbBind(mapping_data_all,as.character(list(auth$user)))
  #     polling_data_all_map <- dbFetch(mapping_data_all)
  #     
  #     return(list(polling_data_map(), polling_data_all_map()))
  #   }
  # )
  #Defining the polling data specifics per the logged in user
  polling_data_map <- reactive({
    mapping_data <- dbSendQuery(con,"SELECT *
                                     FROM polling_data_recent
                                     WHERE [User] = ?")
    dbBind(mapping_data,as.character(list(auth$user)))
    dbFetch(mapping_data)
  })


  polling_data_all_map <- reactive({
    mapping_data_all <- dbSendQuery(con,"SELECT *
                                     FROM polling_data
                                     WHERE [User] = ?")
    dbBind(mapping_data_all,as.character(list(auth$user)))
    dbFetch(mapping_data_all)
  })
  
  observeEvent(input$refresh,{
    progressSweetAlert(
      session = session
      ,id = "refresh_progress",
      title = "Refresh in progress...",
      display_pct = F, value = 100
    )
 
    session$reload()
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title =" Refresh completed !",
      type = "success"
    )
  })
  
  
    polling_data_filtered <- reactive({
    if(is.null(input$county_name_id) & is.null(input$const_name_id) &
       is.null(input$caw_name_id)){
       polling_data_map() %>% 
        dplyr::filter(constituency %in% input$constituency_group &
                        caw_name %in% input$ward_group)# & 
                       # Status %in% input$leaflet_filters)
    } else {
      polling_data_map() %>% 
        dplyr::filter(county_name %in% input$county_name_id &
                        constituency %in% input$constituency_group &
                        caw_name %in% input$ward_group)# &
                        #Status %in% input$leaflet_filters) 
    }
  })
  
  callModule(trackerServer,"tracker_module",user_auth, polling_data_map, polling_data_all_map)
  dataUploadServer('upload_module')
  

  #Leaflet color pallete
  pal <- colorFactor(palette = c("red", "grey", "green"),
                     levels = c("Lost", "Unsure", "Won"))
  
  
  output$my_map <- renderLeaflet ({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      flyToBounds(lng1 = 33.057142,lat1 = -4.696573,lng2 =42.593274, lat2 = 5.03700)  
  })
  
  map_datasets
  map_view_selection
  
  
  output$no_of_wards <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Number of wards",
      value = HTML(paste0(format(polling_data_filtered() %>% 
                                   group_by(caw_name) %>% 
                                   summarise(count = n()) %>% 
                                   ungroup() %>% nrow(),
                                 format = "f", big.mark = ",", nsmall = 0),br(),
                          tags$p((paste0('Won:',
                                         format(polling_data_filtered() %>% 
                                                  filter(Status == "Won") %>% 
                                                  group_by(caw_name) %>% 
                                                  summarise(count = n()) %>% 
                                                  ungroup() %>% nrow(),
                                                format = "f", big.mark = ",", nsmall = 0),
                                         "(",
                                         percent(
                                           (polling_data_filtered() %>% 
                                              filter(Status == "Won") %>% 
                                              group_by(caw_name) %>% 
                                              summarise(count = n()) %>% 
                                              ungroup() %>% nrow()) / 
                                             (polling_data_filtered() %>% 
                                                group_by(caw_name) %>% 
                                                summarise(count = n()) %>% 
                                                ungroup() %>% nrow())
                                         ),
                                         ')')), style = "color:green;")
      )),
      icon = "globe"
    )
  })
  output$reg_voters <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Registered voters",
      status = "info",
      value = HTML(paste0(format(polling_data_filtered() %>%
                                   group_by(caw_name) %>%
                                   summarise(voters = sum(total_voters)) %>%
                                   ungroup() %>% select(voters) %>% unlist() %>%
                                   as.integer() %>%
                                   sum(na.rm = TRUE),
                                 format = "f", big.mark = ",", nsmall = 0),br(),
                          tags$p((paste0('Won:',
                                         format(polling_data_filtered() %>%
                                                  filter(Status == 'Won') %>% 
                                                  group_by(caw_name) %>%
                                                  summarise(voters = sum(total_voters)) %>%
                                                  ungroup() %>% select(voters) %>% unlist() %>%
                                                  as.integer() %>%
                                                  sum(na.rm = TRUE),
                                                format = "f", big.mark = ",", nsmall = 0),
                                         "(",
                                         percent(
                                           (polling_data_filtered() %>%
                                              filter(Status == "Won") %>%
                                              group_by(caw_name) %>%
                                              summarise(voters = sum(total_voters)) %>%
                                              ungroup() %>% select(voters) %>% unlist() %>%
                                              as.integer() %>%
                                              sum(na.rm = TRUE)) /
                                             (polling_data_filtered() %>%
                                                group_by(caw_name) %>%
                                                summarise(voters = sum(total_voters)) %>%
                                                ungroup() %>% select(voters) %>% unlist() %>%
                                                as.integer() %>%
                                                sum(na.rm = TRUE))
                                         ),
                                         ')')), style = "color:black;font-size:80%;")
      )
      ),
      icon = "user-plus"
    )
  })
  output$poll_stations <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Polling stations",
      gradientColor = "danger",
      value = HTML(paste0(format(polling_data_filtered() %>%
                                   nrow(),
                                 format = "f", big.mark = ",", nsmall = 0),br(),
                          tags$p((paste0('Won:',
                                         format(polling_data_filtered() %>%
                                                  filter(Status == "Won") %>%
                                                  nrow(),
                                                format = "f", big.mark = ",", nsmall = 0),
                                         "(",
                                         percent(
                                           (polling_data_filtered() %>%
                                              filter(Status == 'Won') %>%
                                              nrow()) /
                                             (polling_data_filtered() %>%
                                                nrow())
                                         ),
                                         ')')), style = "color:black;")
      )
      ),
      icon = "landmark"
    )
  })
  output$survey_responses <- renderUI({
    bs4InfoBox(
      title = "Respondents",
      gradientColor = "primary",
      value = format(nrow(survey_data_filtered()),
                     format = "f", big.mark = ",", nsmall = 0),
      icon = "pencil-square-o",
      width = NULL
    )
  })
  
  #Survey analysis charts
  output$education_chart_a <- renderPlotly({
    education_chart_data <- survey_data_filtered() %>%
      group_by(`Early Childhood Development Education (ECDE)`) %>%
      summarise(Count = n()) %>%
      mutate(Education = `Early Childhood Development Education (ECDE)`)
    plot_ly(x = education_chart_data$Count,
            y = education_chart_data$Education,
            type = 'bar', orientation = 'h', color = I("maroon")) %>% 
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>% 
      layout(title = "Education Level")
    
  })
  
  output$president_chart_a <- renderPlotly({
    president_data <- survey_data_filtered() %>% 
      group_by(President_Selection) %>% 
      summarise(Count = n()) 
    
    plot_ly(labels = ~president_data$President_Selection,
            values = president_data$Count) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "Presidential Selection",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$party_chart_a <- renderPlotly({
    party_data <- survey_data_filtered() %>% 
      group_by(Party_Affiliation) %>% 
      summarise(Count = n()) 
    
    plot_ly(x = party_data$Count,
            y = party_data$Party_Affiliation,
            type = 'bar', orientation = 'h', color = I("#8bd971")) %>% 
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>% 
      layout(title = "Party Affiliation")
    
  })
  
  output$marital_chart_a <- renderPlotly({
    marital_data <- survey_data_filtered() %>% 
      group_by(`Marital Status`) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      mutate(marital_status = `Marital Status`) %>% 
      arrange(desc(Count))
    
    plot_ly() %>% 
      add_trace(
        type = "funnel",
        y = marital_data$marital_status,
        x = marital_data$Count
      ) %>% 
      layout(title = "Marital Status")
  })
  
  output$pressing_chart_a <- renderPlotly({
    pressing_data <- survey_data %>% 
      group_by(Most_Pressing) %>% 
      summarise(Count = n()) 
    
    plot_ly(y = pressing_data$Most_Pressing,
            x = pressing_data$Count,
            type = 'bar', orientation = 'h', color = I("purple")) %>% 
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE),
             #margin = list(l = 40),
             title = "Emerging issues")
    
  })
  
  observeEvent(input$logout_action, {
    session$reload()
  }) 
  
}

shinyApp(ui, server)

