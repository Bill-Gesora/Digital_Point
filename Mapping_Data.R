 map_filter_selection <- observe({
#   #Filtered_data
  #1.County filters
   counties_filtered <- polling_data_map() %>%
     group_by(county_name,Status) %>%
     summarize(voters = sum(total_voters)) %>%
     arrange(desc(voters)) %>%
     mutate(total_voters = sum(voters)) %>% 
     top_n(1,voters)
    countiesshapefile@data <- countiesshapefile@data %>%  left_join(counties_filtered,
                                                                   by = c("COUNTY_NAM" = "county_name")
   )
  #2.Constituency filtered
  constituency_filtered <- polling_data_filtered() %>%
    group_by(constituency,Status) %>%
    summarize(voters = sum(total_voters)) %>%
    arrange(desc(voters)) %>%
    mutate(total_voters = sum(voters)) %>% 
    top_n(1,voters)
  constituencyshapefile@data <- constituencyshapefile@data %>% left_join(constituency_filtered,
                                                                         by = c("CONSTITUEN" = "constituency")
  )
  #3. Ward filtered
  wards_filtered <- polling_data_filtered() %>%
    group_by(caw_name,Status) %>%
    summarize(voters = sum(total_voters)) %>%
    arrange(desc(voters)) %>%
    mutate(total_voters = sum(voters)) %>% 
    top_n(1, voters)
  wardshapefile@data <- wardshapefile@data %>% left_join(wards_filtered,
                                                         by = c("IEBC_WARDS" = "caw_name")
  )

if (is.not.null(input$county_name_id) & is.null(input$const_name_id) & is.null(input$caw_name_id)) {
  req(input$county_name_id)
  
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
  
  labs <- lapply(seq(nrow(fillconstituencies@data)), function(i) {
    paste0( fillconstituencies@data[i, "CONSTITUEN"], '<br>', 
            format(fillconstituencies@data[i, "total_voters"],
                   format = "f", big.mark = ",", nsmall = 0 )
    ) 
  })

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
                label = lapply(labs, htmltools::HTML)
                )

} else if (is.not.null(input$const_name_id) & is.null(input$caw_name_id)) {
  req(input$county_name_id)
  req(input$const_name_id)
  
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
  
  labs <- lapply(seq(nrow(fillwards@data)), function(i) {
    paste0( fillwards@data[i, "IEBC_WARDS"], '<br>', 
            format(fillwards@data[i, "total_voters"],
                   format = "f", big.mark = ",", nsmall = 0 )
    ) 
  })

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
                label = lapply(labs, htmltools::HTML)
                )

} else if (is.not.null(input$caw_name_id)) {
  req(input$county_name_id)
  req(input$const_name_id)
  req(input$caw_name_id)
  
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
  
  labs <- lapply(seq(nrow(poll_points)), function(i) {
    paste0( poll_points[i, "polling_station"], '<br>', 
            format(poll_points[i, "total_voters"],
                   format = "f", big.mark = ",", nsmall = 0 )
    ) 
  })

  leafletProxy("my_map", data = poll_points) %>%
    clearShapes() %>%
    addPolygons(data=selectedward, weight = 2,
                fillColor = ~pal(Status), color = "black",
                stroke = TRUE, smoothFactor = 0.5,
                fillOpacity = 0.3,
                opacity = 0.1) %>% 
    setView(lng = gettingcentroid$x,lat = gettingcentroid$y, zoom = zoomfactor) %>%
    addCircleMarkers(
      weight = 20,
      radius = poll_points$total_voters^(1/5) ,
      fillColor = ~pal(Status), color = "black",
      stroke = FALSE, fillOpacity = 0.5,
      lng = ~longitude, lat = ~latitude,
      label = lapply(labs, htmltools::HTML)
    )

} else if(is.null(input$county_name_id)){
  labs <- lapply(seq(nrow(countiesshapefile@data)), function(i) {
    paste0( countiesshapefile@data[i, "COUNTY_NAM"], '<br>', 
            format(countiesshapefile@data[i, "total_voters"],
                   format = "f", big.mark = ",", nsmall = 0 )
          ) 
  })
  
  leafletProxy("my_map") %>% 
    clearShapes() %>% 
    clearMarkers() %>% 
    setView(lng = 37.906193,lat = -0.023559, zoom = 5.7) %>% 
    addPolygons(data=countiesshapefile, weight = 2,
                fillColor = ~pal(Status),
                color = "black",
                stroke = TRUE, smoothFactor = 0.5,
                fillOpacity = 0.5,
                highlightOptions =  highlightOptions(color = "white",bringToFront = TRUE),
                group = 2,
                opacity = 0.1,
                label = lapply(labs, htmltools::HTML)
    ) 
}

})

