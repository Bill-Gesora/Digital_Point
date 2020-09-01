filtered_updates <- observe({
  if(is.null(input$county_name_id) & is.null(input$const_name_id) &
     is.null(input$caw_name_id)){
    dt <- reactive({
      polling_data
    })
    
    updateSelectizeInput(session, "const_name_id", choices = NULL, selected = NULL)
    updateSelectizeInput(session, "caw_name_id", choices = NULL, selected = NULL)
    updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
    updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
    updateCheckboxGroupInput(session, "leaflet_filters", choices = unique(dt()$Status), selected = unique(dt()$Status))
  
    } else if (is.not.null(input$county_name_id) & is.null(input$const_name_id) & is.null(input$caw_name_id)) {
    dt <- reactive({
      polling_data %>% 
        dplyr::filter(county_name %in% input$county_name_id)
    })
    
    updateSelectizeInput(session, "const_name_id", choices = unique(dt()$constituency), selected = NULL)
    updateSelectizeInput(session, "caw_name_id", choices = unique(dt()$caw_name), selected = NULL)
    updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
    updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
    updateCheckboxGroupInput(session, "leaflet_filters", choices = unique(dt()$Status), selected = unique(dt()$Status))
    
    } else if (is.not.null(input$county_name_id) & is.not.null(input$const_name_id) & is.null(input$caw_name_id)){
      dt <- reactive({
        polling_data %>% 
          dplyr::filter(constituency %in% input$const_name_id)
      })
      
      updateSelectizeInput(session, "caw_name_id", choices = unique(dt()$caw_name), selected = NULL)
      updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
      updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
      updateCheckboxGroupInput(session, "leaflet_filters", choices = unique(dt()$Status), selected = unique(dt()$Status))
    
      } else if (is.not.null(input$county_name_id) & is.not.null(input$const_name_id) & is.not.null(input$caw_name_id)){
      dt <- reactive({
        polling_data %>% 
          dplyr::filter(caw_name %in% input$caw_name_id)
      })
      updateCheckboxGroupInput(session, "ward_group", choices = unique(dt()$caw_name), selected = unique(dt()$caw_name))
      updateCheckboxGroupInput(session, "constituency_group", choices = unique(dt()$constituency), selected = unique(dt()$constituency))
      updateCheckboxGroupInput(session, "leaflet_filters", choices = unique(dt()$Status), selected = unique(dt()$Status))
      }
})



survey_data_filtered <- reactive({
  survey_data %>% 
    filter(`Employment Status` %in% input$employment_selector &
            Religion %in% input$religion_selector&
           `Age category` %in% input$age_selector&
            Gender %in% input$survey_gender &
            Subcounty %in% input$constituency_group_survey &
            Ward %in% input$ward_group_survey)
  
})

#Functions for adding layers----
# Adding_the shape files
