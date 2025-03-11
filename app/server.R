server <- function(input, output, session){
  # Define location bounds with zoom 
  #location_bounds <- list(
  #  "Florida" = list(lat = 27.8, lng = -81.5, zoom = 7),  
  #  "Puerto Rico" = list(lat = 18.459392, lng = -66.198783, zoom = 9)  
  #)  
  
  # update zone choices based on selected location
  observe({
    req(input$location_input)
    location_folder <- if (input$location_input == "Florida") {
      "Florida_Current_Restored"
    } else {
      "PuertoRico_Current_Restored"
    }
    location <- if (input$location_input == "Florida") {
      "Florida"
    } else {
      "PuertoRico"
    }
    
    # Load the shapefile for the base scenario
    shapefile <- load_shapefile(
      return_period = input$return_interval_input,
      scenario = "base",
      location_folder = location_folder,
      location = location
    )
    
    # Extract unique zones
    zones <- unique(shapefile$zone)
    
    # Update the zone input choices
    updateSelectInput(
      session,
      inputId = "zone_input",
      choices = zones,
      selected = zones[1]  # Default to the first zone
    )
  })
  
  # filter data with reactive({})
  selected_data <- reactive({
    req(input$location_input, input$return_interval_input, input$scenario_input, input$zone_input) # Ensure inputs are available
    
    location_folder <- if (input$location_input == "Florida") {
      "Florida_Current_Restored"
    } else {
      "PuertoRico_Current_Restored"
    }
    
    location <- if (input$location_input == "Florida") {
      "Florida"
    } else {
      "PuertoRico"
    }
    
    base_data <- load_shapefile(input$return_interval_input, 
                                "base", 
                                location_folder = location_folder,
                                location = location)
    # filter base data by the selected zone
    base_data <- base_data[base_data$zone == input$zone_input, ]
    
    df <- load_shapefile(input$return_interval_input, 
                         input$scenario_input,
                         location_folder = location_folder,
                         location = location)
    print(df)
    # filter zone
    df <- df[df$zone == input$zone_input, ]
    list(
      base = base_data, 
      restoration = df
    )
  })
  
  # plot with renderPlot
  output$flood_map_output <- renderLeaflet({ 
    data <- selected_data()
    
    #............FIX THIS................
    # get bounding box to update with selected zone
    bbox <- sf::st_bbox(data$base)
    # Calculate the center of the bounding box
    center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
    center_lng <- (bbox["xmin"] + bbox["xmax"]) / 2
    
    leaflet() %>%  
      addProviderTiles(providers$Esri.WorldStreetMap) %>%  
      #setView(-67.1562347, 18.41090066, zoom = 15) %>% 
      addMiniMap(toggleDisplay = TRUE,
                 minimized = FALSE) %>% 
      # Add base scenario polygons
      addPolygons(
        data = data$base,
        color = "turquoise",  # Base scenario color
        weight = 1,
        fillOpacity = 0.5,  
        group = "Base Scenario",  # Add to a group for layer control
        popup = ~paste("Return Period:", rp, "
",
"Scenario: Base", "
",
"Flood Area (km²):", Area_km)
      ) 
      
      #addCircleMarkers(lng = -81.5, lat = 27.8, popup = "Florida") |>
      #addCircleMarkers(lng = -66.5, lat = 18.2, popup = "Puerto Rico")
  })
  
  # Observe changes in the selected data and update the map
  observe({
    data <- selected_data()
    
    bbox <- sf::st_bbox(data$base)  # Use the base scenario data for the bounding box
    print(bbox)
    
    leafletProxy("flood_map_output") %>%
      clearShapes() %>%
      fitBounds(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      ) %>% 
      addPolygons(
        data = data$base,
        color = 'turquoise',
        weight = 1,
        fillOpacity = 0.5,
        popup = ~paste("Return Period:", rp, "<br>",
                       "Scenario:", scenario, "<br>",
                       "Flood Area (km²):", Area_km)
      ) %>% 
      addPolygons(
        data = data$restoration,  # Restoration scenario data
        color = '#B251F1',  # Use palette for restoration scenario
        weight = 1,
        fillOpacity = 0.6,
        group = "Restoration Scenario",  # Add to a group for layer control
        popup = ~paste("Return Period:", rp, "<br>",
                       "Scenario:", scenario, "<br>",
                       "Flood Area (km²):", Area_km)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c('turquoise', '#B251F1'),
        labels = c("Base Scenario", "Restoration Scenario"),
        title = "Flood Scenarios",
        opacity = 0.7
      )

  })
  
  hurricane_filtered <- reactive({
      hurricane_df <- hurricane_data %>% 
        filter(sublocation %in% input$location) %>% 
        filter(return_interval == as.numeric(input$return_interval))
  })
  
  output$impact_histogram <- renderPlot ({
    hurricane_impact <- hurricane_filtered()  # Get the filtered data
    
    # Check if the filtered data is empty before plotting
    if (nrow(hurricane_impact) == 0) {
      return(NULL)  # If data is empty, don't plot
    }
    
    data_range <- range(hurricane_impact$loss_usd, na.rm = TRUE)
    binwidth <- (data_range[2] - data_range[1]) / 15
    
    ggplot(hurricane_impact, aes(x= sublocation, y = loss_usd, fill = value_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Histogram of Hurricane Losses",
           x = "Locations",
           y = "Loss Amount (in USD)") +
      scale_fill_manual(values = c("economic" = "turquoise", "infrastructure" = "grey")) +
      myCustomTheme() +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust =1))
  })
  
  
  
}