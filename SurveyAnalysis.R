#Reading from the Excel file
library(readxl)

path_excel = "C:/Users/bgesora/OneDrive - Deloitte (O365D)/Documents/BG/Personal Projects/R Projects/Digital Point/Workspace/Data"
survey_data <- read_excel(paste0(path_excel,"/Survey_Analysis.xlsx"), sheet = "Sheet1")

#UI for survey analysis
#Ward checkbox
ward_checkbox_2 <- bs4Box(
  id = "ward_box",
  height = "150px",
  width = "300px",
  title = "Ward",
  checkboxGroupInput(
    inputId = "ward_group_survey",
    label = "",
    choices = unique(survey_data$Ward),
    selected = unique(survey_data$Ward)
  )
)


# Constituency checkbox
constituency_checkbox_2 <- bs4Box(
  id  = "constituency_box",
  height = "150px",
  width = "300px",
  title = "Constituency",
  checkboxGroupInput(
    inputId = "constituency_group_survey",
    label = "",
    choices = unique(survey_data$Subcounty),
    selected = unique(survey_data$Subcounty)
  )
)


#Gender selection
gender_selection <- div(class = "col-sm-3",prettyCheckboxGroup(inputId = "survey_gender",
                                    label = "Gender",
                                    choices = c("Male","Female","Other"),
                                    selected = c("Male","Female","Other"),
                                    inline = TRUE,
                                    icon = icon("user"), 
                                    animation = "tada",
                                    shape = "round",
                                    width = '150%'
                                    ))


#Employment selection
employment_selector <- 
  pickerInput(
  inputId = "employment_selector",
  label = "Employment Status", 
  choices = unique(survey_data$`Employment Status`),
  multiple = TRUE,
  selected = unique(survey_data$`Employment Status`),
  options = list(
    style = "btn-secondary")
)

#Religion selection
religion_selector <- pickerInput(
  inputId = "religion_selector",
  label = "Religion", 
  choices = unique(survey_data$Religion),
  multiple = TRUE,
  selected = unique(survey_data$Religion),
  options = list(
    style = "btn-secondary")
)

# Age input
age_selector <- pickerInput(
            inputId = "age_selector",
            label = "Age Category", 
            choices = unique(survey_data$`Age category`),
            multiple = TRUE,
            selected = unique(survey_data$`Age category`),
            options = list(
              style = "btn-secondary")
          )

#Demograhics
# #Education level insights
# education_chart <- ggplot(survey_data %>% 
#          group_by(`Early Childhood Development Education (ECDE)`) %>% 
#          summarise(Count = n()), aes(y = Count)) +
#   geom_bar(aes(fill = `Early Childhood Development Education (ECDE)`)) +
#   theme_classic() +
#   theme(axis.line.x = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank())
# 
# 
# #Marital status
# marital_status_anal <- survey_data %>% 
#   group_by(`Marital Status`) %>% 
#   summarise(Count = n()) 
# 
# marital_status_chart <-highchart() %>%
#             hc_title(text = "Marital Status",
#                      style = list(fontWeight = "italic",
#                                   fontFamily = "Helvetica-Bold",
#                                   fontSize = "1.5em")) %>%
#             hc_add_series_labels_values(marital_status_anal$`Marital Status`, 
#                                         marital_status_anal$Count, 
#                                         type = "pie",
#                                         size = '120%',
#                                         innerSize = "60%",
#                                         dataLabels = list(enabled = FALSE, 
#                                                           format = '{point.name}')
#             ) %>%
#             hc_plotOptions(series = list(showInLegend = TRUE)) %>% 
#             hc_legend(layout = "vertical", verticalAlign = "bottom",
#                       align = "right", valueDecimals = 0)%>%
#             hc_add_theme(hc_theme_smpl())
# 
# 
# #Religion
# religion_chart <- ggplot(survey_data %>% 
#          group_by(Religion) %>% 
#          summarise(Count = n()), aes(y = Count)) +
#   geom_bar(aes(fill = Religion)) +
#   theme_classic() +
#   theme(axis.line.x = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank())
  
#Employment



#County & Ward





#Age Category


#Insights

#County direction



#Most pressing problems



#Service delivery




# Leaders Approval
  ggplotly(ggplot(as.data.frame(rbind(cbind(Sentiments = survey_data$`Musalia Mudavadi`,
        Candidate = 'Musalia Mudavadi'),
        cbind(Sentiments = survey_data$`Raila Odinga`,
              Candidate = 'Raila Odinga'),
        cbind(Sentiments = survey_data$`Uhuru Kenyatta`,
              Candidate = 'Uhuru Kenyatta'),
        cbind(Sentiments = survey_data$`William Ruto`,
              Candidate = 'William Ruto'),
        cbind(Sentiments = survey_data$`William Ruto`,
              Candidate = 'William Ruto'))
      ) %>% 
        group_by(Candidate,Sentiments) %>% 
      summarize(Count = n()), 
      aes(Candidate, Count)) +
    geom_bar(aes(fill = Sentiments), position = "dodge", stat="identity") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "top"
          )) %>% 
      layout(legend = list(
        orientation = "h",
        xanchor = "center", 
        x = 0.2
      )
  )
    
# Presidential aspiration
  ggplotly(ggplot(survey_data %>% 
    group_by(President_Selection) %>% 
    summarize (Count = n()) %>% 
      arrange(desc(Count)),
    aes(x = President_Selection, y = Count)
    ) + geom_bar(stat = 'identity' , position="dodge", fill = "blue") +
    coord_flip() +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank()
          )
  )


