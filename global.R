# Load required libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(plotly)
library(forecast)

# Load and preprocess data
tryCatch({
  # If you're loading from a CSV file, uncomment and modify this:
  # npl_data <- read.csv("your_data_path.csv")
  
  # Create map data
  npl_map_data <- npl_data %>%
    select(District, Latitude, Longitude) %>%
    distinct()
  
  # Define constants
  excluded_columns <- c("Date", "District", "Longitude", "Latitude")
  
  weather_units <- c(
    "Precipitation" = "mm/day",
    "Pressure" = "kPa",
    "Humidity_2m" = "g/kg",
    "Relative_Humidity_2m" = "%",
    "Temperature_2m" = "°C",
    "Wet_Bulb_Temperature_2m" = "°C",
    "Max_Temperature_2m" = "°C",
    "Min_Temperature_2m" = "°C",
    "Temperature_Range_2m" = "°C",
    "Earth_Skin_Temperature" = "°C",
    "Wind_Speed_10m" = "m/s",
    "Max_Wind_Speed_10m" = "m/s",
    "Min_Wind_Speed_10m"= "m/s",
    "Wind_Speed_Range_10m" = "m/s",
    "Wind_Speed_50m"= "m/s",
    "Max_Wind_Speed_50m" = "m/s",
    "Min_Wind_Speed_50m" = "m/s",
    "Wind_Speed_Range_50m" = "m/s"
  )
}, error = function(e) {
  stop(paste("Error loading data:", e$message))
})




#------------------- Preparing Data for Correlation Tab -------------------------
npl_data$New_Date = as.Date(npl_data$Date)
npl_data$Year = format(as.Date(npl_data$New_Date), "%Y")
npl_data$Month = format(npl_data$New_Date, "%m/%Y")
corr_excluded_columns = c("Date", "New_Date", "District", "Longitude", "Latitude", "Year", "Month")

