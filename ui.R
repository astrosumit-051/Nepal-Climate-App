ui <- navbarPage(
  "Nepal Climate-Data App", id="nav",
  theme = shinythemes::shinytheme("flatly"),
  
  
  
  # ------------------------- Line Graph ------------------------------------
  tabPanel(
    'Climate Trend',
    div(class="tab_one",
        
        tags$head(
          # Include our custom CSS
          includeCSS("style.css")
        ),
        
        sidebarLayout(
          sidebarPanel(
            h3("Visualization Controls", style = "margin-top: 0; margin-bottom: 1.5rem;"),
            
            selectInput('districtLineGraph', 'Choose your District', choices=unique(npl_data$District), multiple=F, selected = "Surkhet"),
            selectInput('weatherLineGraph', 'Select a Weather Observation', choices = names(npl_data)[!names(npl_data) %in% corr_excluded_columns], multiple =F),
            dateRangeInput("date_range", "Select a Date Range", 
                           start = min(npl_data$Date), end = max(npl_data$Date), 
                           min = min(npl_data$Date), max = max(npl_data$Date)),
            pickerInput('peak_observation', 'See Peak Month(s)', choices = c('ON', 'OFF'), selected = 'OFF')
          ),
          mainPanel(
            plotlyOutput('averageLineGraph', height = "800px")
          )
        )
    )
  ),
  
  
  # -------------------- Forecast Chart UI Code --------------------------  
  
  
  tabPanel(
    'Weather Forecast',
    div(class="tab_two",
        
        tags$head(
          # Include our custom CSS
          includeCSS("style.css")
        ),
        
        sidebarLayout(
          sidebarPanel(
            h3("Visualization Controls", style = "margin-top: 0; margin-bottom: 1.5rem;"),
            
            selectInput('districtForecast', 'Choose your District', choices=unique(npl_data$District), multiple=F, selected="Surkhet"),
            selectInput('weatherForecast', 'Select a Weather Observation', choices = names(npl_data)[!names(npl_data) %in% corr_excluded_columns], multiple =F),
            selectInput("time_scale", "Select a Time Period", choices = c("Annually", "Semi-Annually"), multiple=F),
            selectInput('forecastSpan', 'Forecasting for (in Years)', choices = c(1:10), multiple = F)
          ),
          mainPanel(
            plotlyOutput('forecasting', height = "800px", width = "100%")
          )
        )
    )
  ),
  
  
  
  # --------------------------- Correlation Chart UI  -------------------------
  tabPanel(
    "Correlation Chart",
    
    div(class="tab_three",
        
        tags$head(
          # Include our custom CSS
          includeCSS("style.css")
        ),
        
        
        sidebarLayout(
          sidebarPanel(
            h3("Visualization Controls", style = "margin-top: 0; margin-bottom: 1.5rem;"),
            selectInput("corr_weather1", "Choose Weather One", choices = names(npl_data)[!names(npl_data) %in% corr_excluded_columns], selected = "Temperature_2m"),
            selectInput("corr_weather2", "Choose Weather Two", choices = names(npl_data)[!names(npl_data) %in% corr_excluded_columns], selected = "Precipitation"),
            pickerInput("corr_district", "Choose your District(s)", choices = unique(npl_data$District), selected = "Dang", multiple = TRUE),
            dateRangeInput("corr_date_range", "Select a Date Range",
                           start = min(npl_data$Date), end = max(npl_data$Date),
                           min = min(npl_data$Date), max = max(npl_data$Date)),
            radioButtons("corr_time_scale", "Time Period", choices = c("Month", "Year"), selected = "Month"),
            selectInput("corr_trend_type", "Choose your Trend Line Type", choices = c("Linear","Exponential","Logarithmic"), selected="Linear", multiple = FALSE)
          ),
          
          mainPanel(
            plotlyOutput("corr_scatterplot", height="800px", width = "100%")
          )
        )
    )
  ),
  
  
  
  # ------------------------- Map Creative UI -------------------------------  
  tabPanel(
    "Interactive Map", 
    div(class="tab_four",
        
        tags$head(
          # Include our custom CSS
          includeCSS("style.css")
        ),
        
        leafletOutput("nepal_map",width="100%", height="100%"),
        
        absolutePanel(id = "map_controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      
                      h3("Visualization Controls", style = "margin-top: 0; margin-bottom: 1.5rem;"),
                      
                      selectInput("map_district", "Choose your District",
                                  choices = unique(npl_data$District),
                                  selected = "Lamjung", multiple = FALSE),
                      
                      selectInput("map_weather", "Select your weather event",
                                  choices = names(npl_data)[!names(npl_data) %in% corr_excluded_columns],
                                  selected = NULL, multiple = FALSE),
                      
                      dateRangeInput("map_date_range", "Select a date range",
                                     start = min(npl_data$Date), end = max(npl_data$Date), 
                                     min = min(npl_data$Date), max = max(npl_data$Date)),
                      
                      plotOutput("map_plot", width = "290px", height = "250px")
        )
    )
  )
  
  
)