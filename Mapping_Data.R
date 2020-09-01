map_datasets <- reactive({
  #Formatting the counties shape file
  countiesshapefile@data <- countiesshapefile@data %>% left_join(polling_data_map() %>% 
                                                                   group_by(county_name,Status) %>% 
                                                                   summarize(Count = n(),
                                                                             max_longitude = max(longitude),
                                                                             max_latitude = max(latitude),
                                                                             min_longitude = min(longitude),
                                                                             min_latitude = min(latitude)) %>% 
                                                                   arrange(desc(Count)) %>% 
                                                                   top_n(1) %>% 
                                                                   select(county_name, Status, max_longitude, max_latitude, min_longitude, min_latitude),
                                                                 by = c("COUNTY_NAM" = "county_name")
  )
  #formatting the constituencies shapefile
  constituencyshapefile@data <- constituencyshapefile@data %>% left_join(polling_data_map() %>% 
                                                                           group_by(constituency,Status) %>% 
                                                                           summarize(Count = n(),
                                                                                     max_longitude = max(longitude),
                                                                                     max_latitude = max(latitude),
                                                                                     min_longitude = min(longitude),
                                                                                     min_latitude = min(latitude)) %>% 
                                                                           arrange(desc(Count)) %>% 
                                                                           top_n(1) %>% 
                                                                           select(constituency, Status, max_longitude, max_latitude, min_longitude, min_latitude),
                                                                         by = c("CONSTITUEN" = "constituency")
  )
  #formatting the ward shpefile
  wardshapefile@data <- wardshapefile@data %>% left_join(polling_data_map() %>% 
                                                           group_by(caw_name,Status) %>% 
                                                           summarize(Count = n(),
                                                                     max_longitude = max(longitude),
                                                                     max_latitude = max(latitude),
                                                                     min_longitude = min(longitude),
                                                                     min_latitude = min(latitude)) %>% 
                                                           arrange(desc(Count)) %>% 
                                                           top_n(1) %>% 
                                                           select(caw_name, Status, max_longitude, max_latitude, min_longitude, min_latitude),
                                                         by = c("IEBC_WARDS" = "caw_name")
  )
  
})

#Zoom view based on the different selections
zoom_func <- function(selected_category,map_data){
  shape_length <- sum(map_data$Shape_Area)
}

#Selection of the type of view
map_view_selection <- observe({
  
  if (input$map_radio == "Ward View" & is.null(input$county_name_id)) {
    
    wards_filtered <- polling_data_filtered() %>%
      group_by(caw_name,Status) %>%
      summarize(Count = n(),
                max_longitude = max(longitude),
                max_latitude = max(latitude),
                min_longitude = min(longitude),
                min_latitude = min(latitude)) %>%
      arrange(desc(Count)) %>%
      select(caw_name, Status, max_longitude, max_latitude, min_longitude, min_latitude)
    wardshapefile@data <- wardshapefile@data %>% left_join(wards_filtered,
                                                           by = c("IEBC_WARDS" = "caw_name")
    )
    
    leafletProxy('my_map',data = wards_filtered) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      flyToBounds(lng1 = 33.057142,lat1 = -4.696573,lng2 =42.593274, lat2 = 5.03700) %>%
      addPolygons(data=wardshapefile, weight = 2, 
                  fillColor = ~pal(Status), color = "black",
                  stroke = TRUE, smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  group = 3,
                  highlightOptions =  highlightOptions(color = "white",bringToFront = TRUE),
                  opacity = 0.1,
                  label = ~htmlEscape(IEBC_WARDS))
    
    
  } else if (input$map_radio == "Constituency View" & is.null(input$county_name_id)) {
    constituency_filtered <- polling_data_filtered() %>%
      group_by(constituency,Status) %>%
      summarize(Count = n(),
                max_longitude = max(longitude),
                max_latitude = max(latitude),
                min_longitude = min(longitude),
                min_latitude = min(latitude)) %>%
      arrange(desc(Count)) %>%
      select(constituency, Status, max_longitude, max_latitude, min_longitude, min_latitude)
    constituencyshapefile@data <- constituencyshapefile@data %>% left_join(constituency_filtered,
                                                                           by = c("CONSTITUEN" = "constituency")
    )
    
    leafletProxy('my_map',data = constituency_filtered) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>%
      clearMarkers() %>% 
      flyToBounds(lng1 = 33.057142,lat1 = -4.696573,lng2 =42.593274, lat2 = 5.03700) %>%
      addPolygons(data=constituencyshapefile, weight = 2, 
                  fillColor = ~pal(Status), color = "black",
                  stroke = TRUE, smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  group = 2,
                  highlightOptions =  highlightOptions(color = "white",bringToFront = TRUE),
                  opacity = 0.1,
                  label = ~htmlEscape(CONSTITUEN))
    
  } else if (input$map_radio == "Poll_Station View" & is.null(input$county_name_id)) {
    leafletProxy('my_map',data = polling_data_filtered()) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      flyToBounds(lng1 = 33.057142,lat1 = -4.696573,lng2 =42.593274, lat2 = 5.03700) %>% 
      addCircles(
        weight = 20,
        fillColor = ~pal(Status), color = "black",
        stroke = FALSE, fillOpacity = 0.5,
        group = 4,
        lng = ~longitude, lat = ~latitude,
        label = ~as.character(paste0(polling_station))
      )
  } else if (input$map_radio == "County View" & is.null(input$county_name_id)){
    counties_filtered <- polling_data_filtered() %>%
      group_by(county_name,Status) %>%
      summarize(Count = n(),
                max_longitude = max(longitude),
                max_latitude = max(latitude),
                min_longitude = min(longitude),
                min_latitude = min(latitude)) %>%
      arrange(desc(Count)) %>%
      select(county_name, Status, max_longitude, max_latitude, min_longitude, min_latitude)
      countiesshapefile@data <- countiesshapefile@data %>% 
      #   filter(COUNTY_NAM %in% polling_data_filtered()$county_name) %>% 
      left_join(counties_filtered,
                by = c("COUNTY_NAM" = "county_name")
      )
    
    leafletProxy('my_map',data = counties_filtered) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      flyToBounds(lng1 = 33.057142,lat1 = -4.696573,lng2 =42.593274, lat2 = 5.03700) %>%
      addPolygons(data=countiesshapefile, weight = 2, 
                  fillColor = ~pal(Status), color = "black",
                  stroke = TRUE, smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  group = 1,
                  opacity = 0.1,
                  label = ~htmlEscape(COUNTY_NAM))
  }
})

map_filter_selection <- observe({
  #Filtered_data
  #1.County filters
  counties_filtered <- polling_data_filtered() %>%
    group_by(county_name,Status) %>%
    summarize(Count = n(),
              max_longitude = max(longitude),
              max_latitude = max(latitude),
              min_longitude = min(longitude),
              min_latitude = min(latitude)) %>%
    arrange(desc(Count)) %>%
    select(county_name, Status, max_longitude, max_latitude, min_longitude, min_latitude)
  countiesshapefile@data <- countiesshapefile@data %>% 
    #   filter(COUNTY_NAM %in% polling_data_filtered()$county_name) %>% 
    left_join(counties_filtered,
              by = c("COUNTY_NAM" = "county_name")
    )  
  #2.Constituency filtered
  constituency_filtered <- polling_data_filtered() %>%
    group_by(constituency,Status) %>%
    summarize(Count = n(),
              max_longitude = max(longitude),
              max_latitude = max(latitude),
              min_longitude = min(longitude),
              min_latitude = min(latitude)) %>%
    arrange(desc(Count)) %>%
    select(constituency, Status, max_longitude, max_latitude, min_longitude, min_latitude)
  constituencyshapefile@data <- constituencyshapefile@data %>% left_join(constituency_filtered,
                                                                         by = c("CONSTITUEN" = "constituency")
  )
  #3. Ward filtered
  wards_filtered <- polling_data_filtered() %>%
    group_by(caw_name,Status) %>%
    summarize(Count = n(),
              max_longitude = max(longitude),
              max_latitude = max(latitude),
              min_longitude = min(longitude),
              min_latitude = min(latitude)) %>%
    arrange(desc(Count)) %>%
    select(caw_name, Status, max_longitude, max_latitude, min_longitude, min_latitude)
  wardshapefile@data <- wardshapefile@data %>% left_join(wards_filtered,
                                                         by = c("IEBC_WARDS" = "caw_name")
  )
  
  
if (is.not.null(input$county_name_id) & is.null(input$const_name_id) & is.null(input$caw_name_id)) {
  shape_len <-subset(countiesshapefile$Shape_Leng,
                     countiesshapefile$COUNTY_NAM %in% input$county_name_id,
                         select = countiesshapefile$Shape_Leng)
  
  if(is.null(shape_len)){
    print("length is null")
  }else if (shape_len <= 1.44448) {
    zoomfactor = 12
  }else if (shape_len <= 2.5) {
    zoomfactor = 11
  }else if (shape_len <= 5.) {
    zoomfactor = 10
  }else if (shape_len <= 11.55581) {
    zoomfactor = 9
  }else if (shape_len <= 23.11162) {
    zoomfactor = 8
  }
  print(zoomfactor)
  print(shape_len)
  
  selectedcounty <- subset(countiesshapefile,
                           countiesshapefile$COUNTY_NAM %in% input$county_name_id)
  fillconstituencies <- subset(constituencyshapefile,
                               constituencyshapefile$COUNTY_NAM %in% input$county_name_id)
  gettingcentroid <- gCentroid(selectedcounty)
  
  leafletProxy("my_map") %>%
    clearShapes() %>% 
    clearMarkers() %>% 
    setView(lng = gettingcentroid$x,lat = gettingcentroid$y, zoom = zoomfactor) %>% 
    addPolygons(data=fillconstituencies, weight = 2, 
                fillColor = ~pal(Status), color = "black",
                stroke = TRUE, smoothFactor = 0.5,
                fillOpacity = 0.5,
                highlightOptions =  highlightOptions(color = "white",bringToFront = TRUE),
                group = 2,
                opacity = 0.1,
                label = ~htmlEscape(CONSTITUEN))
  
} else if (is.not.null(input$const_name_id) & is.null(input$caw_name_id)) {
  shape_len <-subset(constituencyshapefile$Shape_Leng,
                     constituencyshapefile$CONSTITUEN %in% input$const_name_id)

  if(is.null(shape_len)){
    print("length is null")
  }else if (shape_len <= 1.44448) {
    zoomfactor = 12
  }else if (shape_len <= 2.5) {
    zoomfactor = 11
  }else if (shape_len <= 5.) {
    zoomfactor = 10
  }else if (shape_len <= 11.55581) {
    zoomfactor = 9
  }else if (shape_len <= 23.11162) {
    zoomfactor = 8
  }
  print(zoomfactor)
  print(paste0(shape_len,'constzoom'))

  selectedconstituency <- subset(constituencyshapefile,
                                 constituencyshapefile$CONSTITUEN %in% input$const_name_id)
  fillwards <- subset(wardshapefile,
                               wardshapefile$CONSTITUEN %in% input$const_name_id)
  
  gettingcentroid <- gCentroid(selectedconstituency)

  leafletProxy("my_map") %>%
    clearShapes() %>%
    clearMarkers() %>% 
    setView(lng = gettingcentroid$x,lat = gettingcentroid$y, zoom = zoomfactor) %>%
    addPolygons(data=fillwards, weight = 2,
                fillColor = ~pal(Status), color = "black",
                stroke = TRUE, smoothFactor = 0.5,
                fillOpacity = 0.5,
                highlightOptions =  highlightOptions(color = "white",bringToFront = TRUE),
                opacity = 0.1,
                label = ~htmlEscape(IEBC_WARDS))
  
} else if (is.not.null(input$caw_name_id)) {
  shape_len <-subset(wardshapefile$Shape_Leng,
                     wardshapefile$IEBC_WARDS %in% input$caw_name_id,
                     select = wardshapefile$Shape_Leng)

  if(is.null(shape_len)){
    print("length is null")
  }else if (shape_len <= 1.44448) {
    zoomfactor = 12
  }else if (shape_len <= 2.5) {
    zoomfactor = 11
  }else if (shape_len <= 5.) {
    zoomfactor = 10
  }else if (shape_len <= 11.55581) {
    zoomfactor = 9
  }else if (shape_len <= 23.11162) {
    zoomfactor = 8
  }
  print(zoomfactor)
  print(shape_len)

  selectedward <- subset(wardshapefile,
                                 wardshapefile$IEBC_WARDS %in% input$caw_name_id)
  poll_points <- subset(polling_data_filtered(),
                        caw_name %in% input$caw_name_id)
  gettingcentroid <- gCentroid(selectedward)

  leafletProxy("my_map", data = poll_points) %>%
    clearShapes() %>%
    setView(lng = gettingcentroid$x,lat = gettingcentroid$y, zoom = zoomfactor) %>%
    addCircleMarkers(
      weight = 20,
      radius = 3,
      fillColor = ~pal(Status), color = "black",
      stroke = FALSE, fillOpacity = 0.5,
      lng = ~longitude, lat = ~latitude,
      label = ~as.character(paste0(polling_station))
    )

}
  
})

