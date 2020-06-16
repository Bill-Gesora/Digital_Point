filtered_updates <- observe({
  if(is.null(input$county_name_id) & is.null(input$const_name_id) &
     is.null(input$caw_name_id)){
    dt <- reactive({
      stats_data 
    })
    
    updateSelectizeInput(session, "const_name_id", choices = NULL, selected = NULL)
    updateSelectizeInput(session, "caw_name_id", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
    updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
  
    } else if (is.not.null(input$county_name_id) & is.null(input$const_name_id) & is.null(input$caw_name_id)) {
    dt <- reactive({
      stats_data %>% 
        dplyr::filter(county_name %in% input$county_name_id)
    })
    
    updateSelectizeInput(session, "const_name_id", choices = unique(dt()$constituency), selected = NULL)
    updateSelectizeInput(session, "caw_name_id", choices = unique(dt()$caw_name), selected = NULL)
    updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
    updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
  
    } else if (is.not.null(input$county_name_id) & is.not.null(input$const_name_id) & is.null(input$caw_name_id)){
      dt <- reactive({
        stats_data %>% 
          dplyr::filter(constituency %in% input$const_name_id)
      })
      
      updateSelectizeInput(session, "caw_name_id", choices = unique(dt()$caw_name), selected = NULL)
      updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
      updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
    
      } else if (is.not.null(input$county_name_id) & is.not.null(input$const_name_id) & is.not.null(input$caw_name_id)){
      dt <- reactive({
        stats_data %>% 
          dplyr::filter(caw_name %in% input$caw_name_id)
      })
      updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
      updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
    }
})

clear_filters <- observeEvent(input$reset_id, {
  updateSelectizeInput(session, "county_name_id", choices = unique(stats_data$county_name), selected = NULL)
  updateSelectizeInput(session, "const_name_id", choices = unique(stats_data$constituency), selected = NULL)
  updateSelectizeInput(session, "caw_name_id", choices = unique(stats_data$caw_name), selected = NULL)
  updateCheckboxGroupInput(session, "ward_group", choices = unique(stats_data$caw_name), selected = unique(stats_data$caw_name))
  updateCheckboxGroupInput(session, "constituency_group", choices = unique(stats_data$constituency), selected = unique(stats_data$constituency))
})

stats_data_filtered <- reactive({
  if(is.null(input$county_name_id) & is.null(input$const_name_id) &
     is.null(input$caw_name_id)){
        stats_data %>% 
      dplyr::filter(constituency %in% input$constituency_group &
                      caw_name %in% input$ward_group)
  } else {
    stats_data %>% 
      dplyr::filter(county_name %in% input$county_name_id &
                      constituency %in% input$constituency_group &
                      caw_name %in% input$ward_group) 
  }
})

stats_data_raw_filtered <- reactive({
  stats_data_raw %>% 
    dplyr::filter(constituency %in% input$constituency_group &
                  caw_name %in% input$ward_group) 
  
})


    


      