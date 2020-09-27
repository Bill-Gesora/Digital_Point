library(shiny)
library(shinymanager)
library(bs4Dash)
library(DT)
library(dplyr)


#Setting options
lang <- shinymanager:::language$new()
lang$add(
  "Please authenticate" = "Sign in"
)

# data.frame with credentials info
# define some credentials
credentials <- data.frame(
  user = dbGetQuery(con,
                    "SELECT [user] FROM App_Users"), # mandatory
  password = dbGetQuery(con,
                        "SELECT password FROM App_Users"),
  stringsAsFactors = FALSE
)

# app
ui <- bs4DashPage(
  includeCSS("www/styles.css"),
  
  # authentication module
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
  
  # result of authentication
  #verbatimTextOutput(outputId = "res_auth"),
  
  # classic app
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    verbatimTextOutput("abc"),
    dataTableOutput('plot1')
  )
)

server <- function(input, output, session) {
  
  # authentication module
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  output$abc <- renderPrint({
    auth$user
  })
  
  # classic app
  selectedData <- reactive({
    
    req(auth$result)  # <---- dependency on authentication result
    
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  

  
  polling_data_map <- reactive({
    mapping_data <- dbSendQuery(con,"SELECT *
                                     FROM polling_data_recent
                                     WHERE [User] = ?")
    dbBind(mapping_data,auth$user)
    dbFetch(mapping_data)
  })
  
  polling_data_all_map <- reactive({
    mapping_data_all <- dbSendQuery(con,"SELECT *
                                     FROM polling_data
                                     WHERE [User] = ?")
    dbBind(mapping_data_all,auth$user)
    dbFetch(mapping_data_all)
  })
  
  polling_data_filtered <- reactive({
    polling_data_map() %>% 
      group_by(county_name, Status) %>% 
      summarize(Voters = sum(total_voters)) %>% 
      dplyr::filter(county_name == "NAIROBI CITY") %>% 
      ungroup()
  })
  
  output$plot1 <- renderDataTable({
   polling_data_filtered() %>%
      dplyr::filter(Status == "Won") %>% 
      select(Voters) %>% 
       sum()
   #    polling_data_filtered() %>% 
   #    sum(Voters)

  })
}

shinyApp(ui, server)
