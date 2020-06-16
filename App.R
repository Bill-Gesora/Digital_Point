library(shiny)
library(bs4Dash)
library(odbc) # For creating a connection to the database
library(snakecase)
library(shinyWidgets)
library(htmltools)
library(leaflet)
library(plotly)
library(ggplot2)
library(DT)
library(htmlwidgets)

options(scipen=999)  # turn-off scientific notation like 1e+48 

#Importing other css files
source("Data_Manipulation.R")
source("UI_Widgets.R")

# Changing color based on clicks
javascript <- "
function(el, x){
  el.on('plotly_click', function(data) {
    var colors = [];
    // check if color is a string or array
    if(typeof data.points[0].data.marker.color == 'string'){
      for (var i = 0; i < data.points[0].data.marker.color.length; i++) {
        colors.push(data.points[0].data.marker.color);
      }
    } else {
      colors = data.points[0].data.marker.color;
    }
    // some debugging
    //console.log(data.points[0].data.marker.color)
    //console.log(colors)

    // set color of selected point
    colors[data.points[0].pointNumber] = '#FF00FF';
    Plotly.restyle(el,
      {'marker':{color: colors}},
      [data.points[0].curveNumber]
    );
  });
}
"
#create a custom function
is.not.null <- function(x) !is.null(x)

# setting my working directory
path <- "C:/Users/bgesora/OneDrive - Deloitte (O365D)/Documents/BG/Personal Projects/R Projects/Digital Point/Workspace/Digital_Point_v1"
# connection to the db
con <- dbConnect(odbc::odbc(), "mysqldb")

ui <- bs4DashPage(
  includeCSS("www/styles.css"),
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
  
  output$my_map <- renderPlotly({
    if(input$map_radio == "Constituency View") {
     ggplotly(#height = "80%"
              ggplot(stats_data_filtered(), aes(x = caw_name, y = total_voters,color = "maroon")) +
              geom_point(aes(size = total_voters)) + theme(legend.position = "none",
                                                       axis.title = element_blank(),
                                                       axis.text = element_blank(),
                                                       axis.ticks = element_blank()
                                                      ) 
                        ) %>% onRender(javascript) 
    } else if (input$map_radio == "County View") {
      ggplotly(
        ggplot(stats_data_filtered() %>% 
               group_by(constituency) %>% 
              summarise(total_voters = sum(total_voters)), 
              aes(x = constituency, y = total_voters,color = "maroon")) +
          geom_point(aes(size = total_voters)) + theme(legend.position = "none",
                                                       axis.title = element_blank(),
                                                       axis.text = element_blank(),
                                                       axis.ticks = element_blank()
          ) 
      ) %>% onRender(javascript) 
      
    } else if(input$map_radio == "Ward View") {
      ggplotly(
        ggplot(stats_data_raw_filtered(), aes(x = polling_station_name, 
                                   y = total_voters,color = "red")) +
          geom_point(aes(size = total_voters)) + theme(legend.position = "none",
                                                       axis.title = element_blank(),
                                                       axis.text = element_blank(),
                                                       axis.ticks = element_blank()
          ) 
      ) %>% onRender(javascript)   
      
    } else {
      ggplotly(
        ggplot(stats_data_filtered() %>% 
                 group_by(county_name) %>% 
                 summarise(total_voters = sum(total_voters)), 
                aes(x = county_name, 
                    y = total_voters,color = "red")) +
                    geom_point(aes(size = total_voters)) + theme(legend.position = "none",
                                                       axis.title = element_blank(),
                                                       axis.text = element_blank(),
                                                       axis.ticks = element_blank()
          ) 
      ) %>% onRender(javascript) 
      }
    })
  
  output$no_of_wards <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Number of wards",
      value = format(nrow(stats_data_filtered()),
                     format="f", big.mark=',', nsmall = 0),
      icon = "globe"
    )
  })
  
  output$reg_voters <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Registered voters",
      status = "info",
      value = format(sum(stats_data_filtered()$total_voters),
                     format="f", big.mark=',', nsmall = 0),
      icon = "user-plus"
    )
  })
  
  output$poll_stations <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Polling stations",
      gradientColor = "danger",
      value = format(sum(stats_data_filtered()$polling_stations),
                     format="f", big.mark=',', nsmall = 0),
      icon = "landmark"
    )
  })
  
}

shinyApp(ui, server)
