
#county_name_selector
county_name_selector <- selectizeInput(
  inputId = "county_name_id",
  label = "County", 
  multiple = TRUE,
  choices = unique(polling_data$county_name),
  selected = NULL,
  options = list(placeholder = 'County',
                 plugins = list('remove_button'))
)

#constituency_name_selector
const_name_selector <- selectizeInput(
  inputId = "const_name_id",
  label = "Constituency", 
  multiple = TRUE,
  choices = NULL,
  selected = NULL,
  options = list(placeholder = 'Constituency',
                 plugins = list('remove_button'))
)

#caw_name_selector
caw_name_selector <- selectizeInput(
  inputId = "caw_name_id",
  label = "Ward", 
  multiple = TRUE,
  choices = NULL,
  selected = NULL,
  options = list(placeholder = 'Ward',
                 plugins = list('remove_button'))
)

ward_checkbox <- bs4Box(
  height = "240px",
  width = "300px",
  title = "Ward",
  checkboxGroupInput(
    inputId = "ward_group",
    label = "",
    choices = unique(polling_data$caw_name),
    selected = unique(polling_data$caw_name)
  )
)


constituency_checkbox <- bs4Box(
  height = "240px",
  width = "300px",
  title = "Constituency",
  checkboxGroupInput(
    inputId = "constituency_group",
    label = "",
    choices = unique(polling_data$constituency),
    selected = unique(polling_data$constituency)
  )
)

#navbar
navbar = bs4DashNavbar(
  "We can include cient info here",
  skin = "light",
  status = NULL,
  border = TRUE,
  sidebarIcon = "bars",
  compact = FALSE,
  controlbarIcon = "th",
  leftUi = NULL,
  rightUi = actionButton(inputId = "logout_action", 
                         label = "logout", 
                         icon = NULL, width = NULL)
)

#Absolute panel for leaflet map
#absolutePanel()

#Filters reset button
my_refresh_button <- actionButton(
  inputId = "refresh",
  label = "Refresh",
  styleclass = "primary"
)

#Define colors for leaflet map
pal <- colorFactor(c("red", "grey", "green"),
                   domain = unique(polling_data$Status))


#sidebar
sidebar = bs4DashSidebar(
  inputId = "sidebar",
  disable = FALSE,
  title = "Digital Point",
  skin = "light",
  status = "primary",
  brandColor = "green",
  url = NULL,
  src = "https://t3.ftcdn.net/jpg/03/49/50/42/240_F_349504264_NvpEyjtwOragV7gqAatO6iDj6BPw1tqE.jpg",
  elevation = 4,
  opacity = 0.8,
  expand_on_hover = FALSE,
  sidebar_collapsed = FALSE,
  
  #Adding user icon here
  bs4SidebarUserPanel(img = "https://image.flaticon.com/icons/svg/3021/3021989.svg", 
                      text = textOutput(outputId = "res_auth")),
  
  #creating the sidebar menu
  bs4SidebarMenu(
    id = "sidebarmenu",
    flat = FALSE,
    compact = TRUE,
    child_indent = TRUE,
    # bs4SidebarHeader(NULL),
    bs4SidebarMenuItem(
      text = "My Dashboard",
      tabName = "dashboard",
      icon = "chart-line",
      startExpanded = FALSE
    ),
    bs4SidebarMenuItem(
      text = "Project Tracker",
      tabName = "tracker",
      icon = "bar-chart",
      startExpanded = FALSE
    ),
    bs4SidebarMenuItem(
      text = "Twitter Analysis",
      tabName = "twitter",
      icon = "twitter",
      startExpanded = FALSE
    ),
    bs4SidebarMenuItem(
      text = "Whatsapp Analysis",
      tabName = "whatsapp",
      icon = "whatsapp",
      startExpanded = FALSE
    ),
    bs4SidebarMenuItem(
      text = "Facebook Analysis",
      tabName = "facebook",
      icon = "facebook",
      startExpanded = FALSE
    ),
    bs4SidebarMenuItem(
      text = "Survey Analysis",
      tabName = "surveys",
      icon = "tasks",
      startExpanded = FALSE
    )
  )
)


#footer
footer = bs4DashFooter()

#body
body = bs4DashBody(
  bs4TabItems(
    bs4TabItem (
      tabName = "dashboard", 
      fluidRow(
        column(4, county_name_selector),
        column(4, const_name_selector),
        column(3, caw_name_selector ),
        column(1, my_refresh_button)
      ),
      fluidRow(id = "fluidRow1",
               column(3,
                      constituency_checkbox,
                      ward_checkbox
               ),
               column(9, 
                      bs4Box(
                        absolutePanel( top = 25,right = 2, draggable = TRUE,
                                       awesomeRadio(
                                         inputId = "map_radio",
                                         label = NULL,
                                         choices = c("County View","Constituency View", "Ward View", "Poll_Station View"),
                                         selected = "County View",
                                         inline = TRUE,
                                         checkbox = TRUE
                                       )
                        ),
                        height = "400px",
                        width = "600px",
                        title = NULL,
                        leafletOutput(outputId = "my_map"),
                        absolutePanel(id = "leaflet_filters_pane", top = 40,right = 23, draggable = TRUE,
                                      width = '150px',
                                      awesomeCheckboxGroup(
                                        inputId = "leaflet_filters",
                                        label = NULL, 
                                        choices = unique(polling_data$Status),
                                        selected = unique(polling_data$Status)
                                      )
                        )
                      ),
                      fluidRow(
                        bs4InfoBoxOutput("no_of_wards"),
                        bs4InfoBoxOutput("reg_voters" ),
                        bs4InfoBoxOutput("poll_stations")
                      )
               )
      )
    ),bs4TabItem(
      tabName = "tracker",
      trackerUI('tracker_module')
    ),
    bs4TabItem(
      tabName = "twitter", 
      fluidRow(
        column(4,
               bs4Box(
                 height = "540px",
                 width = "600px",
                 title = "Trending topics" ,
                 ggplotly(text_clean %>%
                            count(word, sort = TRUE) %>%
                            top_n(10) %>%
                            mutate(word = reorder(word, n)) %>%
                            ggplot(aes(x = word, y = n)) +
                            geom_col(fill = "blue") +
                            xlab(NULL) +
                            coord_flip() +
                            labs(y = NULL,
                                 x = NULL,
                                 title = NULL) +
                            theme_minimal() +
                            theme(axis.text.x = element_blank(),
                                  legend.position = "none")
                 )
               )
        ),
        column(8,
               column(12,
                      bs4Box(
                        height = "260px",
                        width = "600px",
                        title = "Politicians popularity" ,
                        ggplotly(height = 220,
                                 ggplot(as.data.frame(busia_politicians %>%
                                                        group_by(politician) %>% 
                                                        summarise(Mentions = n())),
                                        aes(x = politician, y = Mentions)
                                 ) + geom_bar(stat = "identity",fill="steelblue") +
                                   theme(axis.title = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank())
                        )
                      )
               ),
               column(12,
                      bs4Box(
                        height = "260px",
                        width = "600px",
                        title = "Tweeters' sentiments" ,
                        ggplotly(height = 220,sentiments_tweet)
                      )
               )
        )
      )
      
    ),
    bs4TabItem(
      tabName = "whatsapp",
      dataUploadUI('upload_module')
    ),
    bs4TabItem(
      tabName = "facebook"
    ),
    bs4TabItem(
      tabName = "surveys",
      fluidRow(
        column(3,employment_selector),
        column(3,religion_selector),
        column(3,age_selector),
        column(2,gender_selection,)
      ),
      fluidRow(
        column(3,
               constituency_checkbox_2,
               ward_checkbox_2,
               uiOutput("survey_responses")
        ),
        column(9,
               grillade(
                 plotlyOutput("education_chart_a", height = "255px"),
                 plotlyOutput("president_chart_a", height = "255px"),
                 plotlyOutput("party_chart_a", height = "255px")
                 
               ),
               grillade(
                 plotlyOutput("marital_chart_a", height = "255px"),
                 plotlyOutput("pressing_chart_a", height = "255px")
               )
        )
      )
    )
  )
)