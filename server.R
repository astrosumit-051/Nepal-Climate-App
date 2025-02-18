server <- function(input, output, session) {
  
  # -------------------------- Map Chart Server Code --------------------------------
  
  
  
  # Reactive expression for district averages
  district_averages <- reactive({
    req(input$map_weather, input$map_date_range)
    
    tryCatch({
      npl_data %>%
        filter(as.Date(Date) >= as.Date(input$map_date_range[1]) &
                 as.Date(Date) <= as.Date(input$map_date_range[2])) %>%
        group_by(District) %>%
        summarize(map_avg_weather = mean(!!sym(input$map_weather), na.rm = TRUE)) %>%
        left_join(npl_map_data, by = "District")
    }, error = function(e) {
      showNotification(paste("Error calculating averages:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Initial map render
  output$nepal_map <- renderLeaflet({
    req(district_averages(), input$map_weather)
    
    data <- district_averages()
    
    # Create color palette
    pal <- colorNumeric(
      palette = "Blues",
      domain = data$map_avg_weather,
      na.color = "#d9d9d9"
    )
    
    leaflet(data = data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 84.1240, lat = 28.3949, zoom = 8) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 8,
        layerId = ~District,
        fillColor = ~pal(map_avg_weather),
        color = "grey",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.9,
        popup = ~sprintf(
          "<div style='font-family: Arial; padding: 5px;'>
           <h4 style='margin: 0 0 5px 0;'>%s</h4>
           <b>%s:</b> %.2f %s</div>",
          District,
          input$map_weather,
          map_avg_weather,
          weather_units[[input$map_weather]]
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ~map_avg_weather,
        title = paste(input$map_weather, 
                      ifelse(input$map_weather %in% names(weather_units), 
                             paste0(" (", weather_units[[input$map_weather]], ")"), 
                             "")),
        opacity = 0.8
      )
  })
  
  # Observer for map marker clicks
  observeEvent(input$nepal_map_marker_click, {
    click <- input$nepal_map_marker_click
    if (!is.null(click)) {
      updateSelectInput(session, "map_district", selected = click$id)
    }
  })
  
  # Map updates
  observe({
    req(district_averages(), input$map_weather, input$map_district)
    
    data <- district_averages()
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = data$map_avg_weather,
      na.color = "#d9d9d9"
    )
    
    leafletProxy("nepal_map") %>%
      clearGroup(c("markers", "labels")) %>%
      addCircleMarkers(
        data = data,
        lng = ~Longitude,
        lat = ~Latitude,
        layerId = ~District,
        group = "markers",
        fillColor = ~pal(map_avg_weather),
        # color = "grey",
        color = ~ifelse(District == input$map_district, "red", "white"),
        fillOpacity = ~ifelse(District == input$map_district, 0.9, 0.9),
        weight = ~ifelse(District == input$map_district, 2, 1),
        radius = 8,
        popup = ~sprintf(
          "<div style='font-family: Arial; padding: 5px;'>
           <h4 style='margin: 0 0 5px 0;'>%s</h4>
           <b>%s:</b> %.2f %s</div>",
          District,
          input$map_weather,
          map_avg_weather,
          weather_units[[input$map_weather]]
        )
      )
    
    # Add label for selected district
    if (!is.null(input$map_district)) {
      selected_data <- data %>% filter(District == input$map_district)
      leafletProxy("nepal_map") %>%
        addLabelOnlyMarkers(
          data = selected_data,
          lng = ~Longitude,
          lat = ~Latitude,
          group = "labels",
          label = ~District,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = 'top',
            offset = c(0, -15),
            textsize = "14px",
            style = list(
              "color" = "red",
              "font-weight" = "bold",
              "background-color" = "white",
              "padding" = "3px 8px",
              "border" = "2px solid red",
              "border-radius" = "4px",
              "box-shadow" = "2px 2px 4px rgba(0,0,0,0.2)"
            )
          )
        )
    }
  })
  
  # Line chart
  output$map_plot <- renderPlot({
    req(input$map_weather, input$map_date_range, input$map_district)
    
    tryCatch({
      district_data <- npl_data %>%
        filter(District %in% input$map_district &
                 as.Date(Date) >= input$map_date_range[1] &
                 as.Date(Date) <= input$map_date_range[2]) %>%
        group_by(Month) %>%
        summarize(map_avg_weather = mean(!!sym(input$map_weather), na.rm = TRUE), 
                  .groups = "drop") %>%
        mutate(Month = lubridate::my(Month))
      
      if (nrow(district_data) == 0) {
        return(NULL)
      }
      
      ggplot(district_data, aes(x = Month, y = map_avg_weather)) +
        geom_smooth(method = "loess", color = "#2290e5", 
                    fill = "lightblue", alpha = 0.3, se = TRUE) +
        labs(x = "Date",
             y = paste("Average", input$map_weather)) +
        theme_classic()
      
    }, error = function(e) {
      showNotification(paste("Error creating plot:", e$message), type = "error")
      return(NULL)
    })
  })
  
  



  
  
  
  
  
  # ----------------------------------- Correlation Chart Server Code ----------------------------------------------
  
  
  
  data_summary <- reactive({
    
    
    npl_data %>%
      filter(District %in% input$corr_district & 
               New_Date >= input$corr_date_range[1] & 
               New_Date <= input$corr_date_range[2]) %>%
      group_by(District, !!sym(input$corr_time_scale)) %>%
      summarize(
        Avg_Weather1 = mean(!!sym(input$corr_weather1), na.rm = TRUE),
        Avg_Weather2 = mean(!!sym(input$corr_weather2), na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  output$corr_scatterplot <- renderPlotly({
    req(data_summary())
    p <- ggplot(data_summary(), aes(x = Avg_Weather1, y = Avg_Weather2, color = District)) +
      geom_point()
    
    if (input$corr_trend_type == "Linear") {
      corr_trend_line <- geom_smooth(method = "lm", formula =( y ~ x), 
                                     # aes(group = District), 
                                     se = FALSE)
    } else if (input$corr_trend_type == "Exponential") {
      corr_trend_line <- geom_smooth(method = "lm", formula = (y ~exp(-x)), 
                                     # method.args = list(start = list(a = 0, b = 0)),
                                     # aes(group = District),
                                     se = FALSE)
    } else if (input$corr_trend_type == "Logarithmic") {
      corr_trend_line <- geom_smooth(method = "lm", formula = (y ~ log(x)),
                                     # method.args = list(start = list(a = 0, b = 1)),
                                     # aes(group = District), 
                                     se = FALSE)
    }
    
    p + corr_trend_line + 
      labs(x = paste("Average", input$corr_weather1),
           y = paste("Average", input$corr_weather2),
           title = paste("Correlation between", input$corr_weather1, "and", input$corr_weather2),
           subtitle = paste("Grouped by", tolower(input$corr_time_scale))) +
      theme_classic()+
      theme(axis.title.y =element_text(color='lightsteelblue4'), axis.title.x = element_text( color='lightsteelblue4'),
            axis.text = element_text(color='lightsteelblue4'), axis.line = element_line( color='lightsteelblue4'), axis.ticks = element_line(color='lightsteelblue4'),
            panel.background = element_rect(fill='white'), title = element_text(size=13,color='lightsteelblue4'))
      # annotate("text", x = 16.5, y = 3.5,
      #                            label = "As temperature increases the level of \n precipitation decreases, leading to droughts" , color="black",
      #                            size=4 , angle=45, fontface="bold")
  })
  
  
  
  
  # -------------------- Forecast Chart Server Code ----------------------------------
  
  output$forecasting = renderPlotly({
    req(input$districtForecast, input$weatherForecast, input$forecastSpan, input$time_scale)
    
    # Validate and prepare data
    processed_data <- 
      if (input$time_scale == "Semi-Annually") {
        # Semi-annual data preparation
        npl_data %>%
          filter(District == input$districtForecast) %>%
          select(Date, !!sym(input$weatherForecast)) %>%
          mutate(Date = as.Date(Date)) %>% # Ensure Date is of Date class
          arrange(Date) %>%
          filter(!is.na(!!sym(input$weatherForecast))) %>%
          mutate(HalfYear = ifelse(month(Date) <= 6,
                                   floor_date(Date, "year"),
                                   floor_date(Date, "year") + months(6))) %>%
          group_by(HalfYear) %>%
          summarise(Average_Value = round(mean(as.numeric(!!sym(input$weatherForecast))),2), na.rm = TRUE) %>%
          rename(Date = HalfYear)
      } else {
        # Yearly data preparation
        npl_data %>%
          filter(District == input$districtForecast) %>%
          select(Date, !!sym(input$weatherForecast)) %>%
          mutate(Date = as.Date(Date)) %>% # Ensure Date is of Date class
          arrange(Date) %>%
          filter(!is.na(!!sym(input$weatherForecast))) %>%
          mutate(Year = floor_date(Date, "year")) %>%
          group_by(Year) %>%
          summarise(Average_Value = round(mean(as.numeric(!!sym(input$weatherForecast))),2), na.rm = TRUE) %>%
          rename(Date = Year)
      }
    
    
    # Ensure Date column is Date type
    processed_data <- processed_data %>%
      mutate(Date = as.Date(Date))
    
    # Determine frequency
    freq <- if (input$time_scale == "Semi-Annually") 2 else 1
    
    # Create time series
    ts_data <- ts(processed_data$Average_Value,
                  start = year(min(processed_data$Date)),
                  frequency = freq)
    
    # Fit ARIMA model
    arima_model <- 
      auto.arima(ts_data,
                 seasonal = (freq > 1),
                 stepwise = TRUE,
                 approximation = FALSE)
    
    
    # Generate forecast
    forecast_result <- forecast(arima_model,
                                h = as.numeric(input$forecastSpan) * freq)
    
    # Get last historical date
    last_hist_date <- as.Date(max(processed_data$Date))
    
    # Create forecast dates
    forecast_dates <- seq.Date(
      from = last_hist_date,
      by = if (freq == 2) "6 months" else "1 year",
      length.out = as.numeric(input$forecastSpan) * freq
    )
    
    # Get last historical point
    last_hist_value <- processed_data$Average_Value[processed_data$Date == last_hist_date]
    
    # Create forecast data frame
    forecast_df <- data.frame(
      Date = forecast_dates,
      Average_Value = round(as.numeric(forecast_result$mean),2),
      Lower = as.numeric(forecast_result$lower[, 2]),
      Upper = as.numeric(forecast_result$upper[, 2])
    )
    
    # Create connecting line data frame
    connecting_line <- data.frame(
      x = c(last_hist_date, forecast_dates[1]),
      y = c(last_hist_value, forecast_result$mean[1])
    )
    
    # Create plot using ggplot2
    p <- ggplot() +
      # Historical data
      geom_line(data = processed_data, aes(x = Date, y = Average_Value, color = "Historical")) +
      # Connecting line
      geom_line(data = connecting_line, aes(x = x, y = y), 
                linetype = "dashed", color = "#2290e5") +
      # Forecast line
      geom_line(data = forecast_df, aes(x = Date, y = Average_Value, color = "Forecast"), 
                linetype = "dashed") +
      # Confidence interval
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), 
                  fill = "lightblue", alpha = 0.3) +
      # Color settings
      scale_color_manual(values = c("Historical" = "lightblue", "Forecast" = "#2290e5")) +
      labs(
        
        title = paste(input$time_scale, "Forecast of", input$weatherForecast, "for", input$districtForecast),
        x = "Date",
        y = paste("Average", input$weatherForecast),
        color = "Data Type"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "semibold"),
        axis.title = element_text(),
        axis.text = element_text(),
        legend.position = "none"
      )+
      theme(axis.title.y =element_text(color='lightsteelblue4'), axis.title.x = element_text( color='lightsteelblue4'),
            axis.text = element_text(color='lightsteelblue4'), axis.line = element_line( color='lightsteelblue4'), axis.ticks = element_line(color='lightsteelblue4'),
            panel.background = element_rect(fill='white'), title = element_text(size=13,color='lightsteelblue4'))
    # +
    #   scale_x_date(
    #     date_breaks = "5 years",  # Break every 5 years
    #     date_labels = "%Y" ,     # Display as 4-digit year
    #     limits = c(as.Date("1990-01-01"), max(c(processed_data$Date, forecast_df$Date)))
    #   )
    
    ggplotly(p)
  })
  
  
  
  
  # --------------------------- Line chart Server Code --------------------------------------
  
  # --------------------------- Line chart Server Code --------------------------------------
  
  
  
  output$averageLineGraph = renderPlotly({
    
    # When "ON" is selected for peak observation, plot by month
    if (input$peak_observation == "ON") {
      # Ensure Month is extracted from Date
      new_data <- subset(npl_data, District %in% input$districtLineGraph & Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        group_by(Month) %>%
        summarize(average_monthly_value = round(mean(!!sym(input$weatherLineGraph), na.rm = TRUE),2))
      
      # Convert Month to a factor to treat it as categorical
      new_data$Month <- factor(new_data$Month, levels = unique(new_data$Month))
      
      # Plot monthly trend (without points)
      plot <- ggplot(new_data, aes(x = Month, y = average_monthly_value, group = 1)) +
        geom_line(color = "#2290e5") +  # Only the line, no points
        theme_classic() +
        labs(title = paste("Peak Months of", input$weatherLineGraph, "from", input$date_range[1], "to", input$date_range[2]), 
             x = 'Month', 
             y = paste('Average', input$weatherLineGraph)) +
        theme(axis.title.y = element_text(color = 'lightsteelblue4'), 
              axis.title.x = element_text(color = 'lightsteelblue4'),
              axis.text = element_text(color = 'lightsteelblue4'), 
              axis.line = element_line(color = 'lightsteelblue4'), 
              axis.ticks = element_line(color = 'lightsteelblue4'), 
              panel.background = element_rect(fill = 'white'), 
              title = element_text(size = 13, color = 'lightsteelblue4')) +
        scale_x_discrete() 
    } else {
      new_data <- subset(npl_data, District %in% input$districtLineGraph & Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        group_by(Year) %>%
        summarize(average_line_graph = round(mean(!!sym(input$weatherLineGraph), na.rm = TRUE),2))
      
      # Identify lowest, highest, and last data points
      lowest_point <- new_data %>% filter(average_line_graph == min(average_line_graph, na.rm = TRUE))
      highest_point <- new_data %>% filter(average_line_graph == max(average_line_graph, na.rm = TRUE))
      last_year <- max(new_data$Year)
      last_point <- new_data %>% filter(Year == last_year)
      plot <- ggplot(new_data, aes(x = Year, y = average_line_graph, group = 1)) +
        geom_line(color = "lightblue") +
        geom_point(data = lowest_point, aes(x = Year, y = average_line_graph), color = "#2290e5", size = 3) +
        geom_point(data = highest_point, aes(x = Year, y = average_line_graph), color = "#2290e5", size = 3) +
        geom_point(data = last_point, aes(x = Year, y = average_line_graph), color = "#2290e5", size = 3) +
        geom_text(data = lowest_point, aes(x = Year, y = average_line_graph + 0.2, label = paste0(round(average_line_graph, 2))), color = "#2290e5", size = 4) +
        geom_text(data = highest_point, aes(x = Year, y = average_line_graph + 0.2, label = paste0(round(average_line_graph, 2))), color = "#2290e5", size = 4) +
        geom_text(data = last_point, aes(x = Year, y = average_line_graph + 0.2, label = paste0(round(average_line_graph, 2))), color = "#2290e5", size = 4) +
        annotate(
          'text',
          x = 5, # Place near the right of the plot
          y = 5, # Slightly below the top value
          hjust = 0,
          label = paste0(
            "There was an increase of\n",
            round((((highest_point$average_line_graph[[1]] - lowest_point$average_line_graph[[1]]) / lowest_point$average_line_graph[[1]]) - 1) * 100, 2), 
            "% over the span of\n",
            as.numeric(highest_point$Year[[1]]) - as.numeric(lowest_point$Year[[1]]),
            " years from ",
            lowest_point$Year[[1]],
            " to ",
            highest_point$Year[[1]]
          ),
          color = 'lightsteelblue4'
        ) +
        theme_classic() +
        theme(plot.title = element_text(face = "semibold")) +
        labs(title = paste("Trend of",input$weatherLineGraph,"for",input$districtLineGraph), 
             x = 'Time', 
             y = paste('Average ', input$weatherLineGraph)) +
        scale_x_discrete(
          breaks = seq(min(npl_data$Year), max(npl_data$Year), by = 5)
        ) +
        theme(axis.title.y = element_text(color = 'lightsteelblue4'), axis.title.x = element_text(color = 'lightsteelblue4'),
              axis.text = element_text(color = 'lightsteelblue4'), axis.line = element_line(color = 'lightsteelblue4'), 
              axis.ticks = element_line(color = 'lightsteelblue4'), panel.background = element_rect(fill = 'white'), 
              title = element_text(size = 13, color = 'lightsteelblue4'))
    }
    ggplotly(plot)
  })
  
  
}

