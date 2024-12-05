library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# UI
ui <- fluidPage(
  tags$head(
    shinyjs::useShinyjs()),
  titlePanel("Vancouver Parking Meter Explorer"),
  tabsetPanel(id = "mainTabset",
              # Tab 1: Main Map
              tabPanel("Parking Meter Map",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("area", "Select Area:", 
                                       choices = NULL),  
                           checkboxInput("credit_card", "Accepts Credit Card", FALSE),
                           checkboxInput("show_motorcycle", "Show Motorcycle Parking", FALSE)),
                         mainPanel(
                           leafletOutput("map", height = "500px")))),
              tabPanel("Pricing Comparison",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("area1", "Select Area 1:", choices = NULL),
                           selectInput("area2", "Select Area 2:", choices = NULL),
                           radioButtons("time_period", "Select Time Period:",
                                        choices = c("Weekday (9AM-6PM)" = "weekday",
                                                    "Weekend (9AM-6PM)" = "weekend"),)),
                         mainPanel(
                           plotOutput("priceComparisonPlot")))),),)

# Server
server <- function(input, output, session) {

  parking_data <- read_delim("https://opendata.vancouver.ca/explore/dataset/parking-meters/download/?format=csv&timezone=America/Los_Angeles&lang=en&use_labels_for_header=true&csv_separator=%3B") %>%
    separate(geo_point_2d, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
    mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    mutate(hourly_rate = as.numeric(gsub("\\$", "", R_MF_9A_6P))) %>% # Extract numeric rates
    filter(!is.na(latitude) & !is.na(longitude))
  
  motorcycle_data <- read_delim("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/motorcycle-parking/exports/csv?lang=en&timezone=America%2FLos_Angeles&use_labels=true&delimiter=%3B") %>%
    separate(geo_point_2d, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
    mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    filter(!is.na(latitude) & !is.na(longitude))
  
  parking_data <- parking_data %>% mutate(type = "parking_meter")
  motorcycle_data <- motorcycle_data %>% mutate(type = "motorcycle")
  
  parking_data <- parking_data %>%
    mutate(
      weekday_rate = as.numeric(gsub("\\$", "", R_MF_9A_6P)),
      weekend_rate = as.numeric(gsub("\\$", "", R_SA_9A_6P)))
  
  selected_meter <- reactiveVal(NULL)

  observe({
    areas <- c("All", sort(unique(parking_data$`Geo Local Area`)))
    updateSelectInput(session, "area", choices = areas, selected = "All")})

  filtered_data <- reactive({
    req(input$area)
    
    data <- if (input$area == "All") {
      parking_data} else {parking_data %>% 
          filter(`Geo Local Area` == input$area)}
    
    if (input$credit_card) {
      data <- data %>% filter(CREDITCARD == "Yes")}
    
    if (input$show_motorcycle) {
      motorcycle <- motorcycle_data %>% 
        mutate(type="motorcycle", `Geo Local Area`="Motorcycle Parking", R_MF_9A_6P="N/A", CREDITCARD="N/A")
      
      data <- bind_rows(data, motorcycle)}
    
    data})
  
  selected_area_bounds <- reactive({
    req(input$area)
    if (input$area == "All") {
      list(
        lng1 = min(parking_data$longitude, na.rm = TRUE),
        lat1 = min(parking_data$latitude, na.rm = TRUE),
        lng2 = max(parking_data$longitude, na.rm = TRUE),
        lat2 = max(parking_data$latitude, na.rm = TRUE))} 
    else {
      area_data <- parking_data %>% filter(`Geo Local Area` == input$area)
      list(
        lng1 = min(area_data$longitude, na.rm = TRUE),
        lat1 = min(area_data$latitude, na.rm = TRUE),
        lng2 = max(area_data$longitude, na.rm = TRUE),
        lat2 = max(area_data$latitude, na.rm = TRUE))}})

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -123.1207, lat = 49.2827, zoom = 11)})

  observe({
    req(filtered_data())
    
    bounds <- selected_area_bounds()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data=filtered_data()%>%filter(type=="parking_meter"),
        lng=~longitude,
        lat=~latitude,
        radius=4,
        color="thistle1",
        fillColor="tomato",
        fillOpacity=0.8,
        popup=~paste(
          "<div style='font-family: Arial, sans-serif; font-size: 12px;'>",
          "<h3 style='margin-top: 0;'>Meter Details</h3>",
          "<b>Neighbourhood:</b> ", `Geo Local Area`, "<br>",
          "<b>Hourly Rate (9AM-6PM):</b> $", hourly_rate, "<br>",
          "<b>Credit Card?:</b> ", CREDITCARD, "<br>",
          "<b>Pay by Phone Meter ID:</b> ", PAY_PHONE, "<br>",
          "</div>"),
        clusterOptions=markerClusterOptions(
          spiderfyOnMaxZoom=TRUE,
          showCoverageOnHover=TRUE,
          zoomToBoundsOnClick=TRUE,
          maxClusterRadius=30)) %>%
      addCircleMarkers(
        data=filtered_data()%>%filter(type=="motorcycle"),
        lng=~longitude,
        lat=~latitude,
        radius=4,
        color="thistle1",
        fillColor="cornflowerblue",
        fillOpacity=0.8,
        popup=~paste("<div style='font-family: Arial, sans-serif; font-size: 12px;'>",
                     "<h3 style='margin-top: 0;'>Meter Details</h3>",
                     "<b>Motorcyle Parking</b> ",
                     "</div>")) %>%
      fitBounds(
        lng1=bounds$lng1, lat1=bounds$lat1,
        lng2=bounds$lng2, lat2=bounds$lat2)})
 
  observe({
    areas <- c("All", sort(unique(parking_data$`Geo Local Area`)))
    updateSelectInput(session, "area1", choices = areas, selected = "NULL")
    updateSelectInput(session, "area2", choices = areas, selected = "NULL")})

  filtered_comparison_data <- reactive({
    req(input$area1, input$area2, input$time_period)
    
    rate_column <- if (input$time_period == "weekday") {"weekday_rate"} else {"weekend_rate"}
    
    area1_data <- parking_data %>%
      filter(`Geo Local Area` == input$area1) %>%
      select(`Geo Local Area`, !!sym(rate_column)) %>%
      mutate(Area = input$area1)
    
    area2_data <- parking_data %>%
      filter(`Geo Local Area` == input$area2) %>%
      select(`Geo Local Area`, !!sym(rate_column)) %>%
      mutate(Area = input$area2)
    
    bind_rows(area1_data, area2_data)})
  
  output$priceComparisonPlot <- renderPlot({
    req(filtered_comparison_data())
    
    comparison_data <- filtered_comparison_data()
    
    ggplot(comparison_data, aes(x = Area, y = !!sym(ifelse(input$time_period == "weekday",
                                                           "weekday_rate",
                                                           "weekend_rate")))) +
      geom_boxplot(fill = c("orchid1", "cornflowerblue"), color = "slategrey") +
      labs(title = paste("Price Comparison:", input$time_period),
           x = "Area",
           y = "Hourly Rate ($)") +
      scale_y_continuous(limits = c(0, 12), breaks = seq(0, 15, by = 2)) +
      theme_minimal()})}

shinyApp(ui=ui, server=server)