server <- function(input, output){
  # filter data with reactive({})
  selected_data <- reactive({
    req(input$return_interval_input, input$scenario_input) # Ensure inputs are available
    base_data <- load_shapefile(input$return_interval_input, "base")
    df <- load_shapefile(input$return_interval_input, input$scenario_input)
    print(df)
    list(
      base = base_data, 
      restoration = df
    )
  })
  
  # plot with renderPlot
  output$flood_map_output <- renderLeaflet({ 
    data <- selected_data()
    leaflet() %>%  
      addProviderTiles(providers$Esri.WorldStreetMap) %>%  
      setView(-66.198783, 18.459392, zoom = 15) %>% 
      addMiniMap(toggleDisplay = TRUE,
                 minimized = FALSE) %>% 
      # Add base scenario polygons
      addPolygons(
        data = data$base,
        color = "darkblue",  # Base scenario color
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
    leafletProxy("flood_map_output") %>%
      clearShapes() %>%
      addPolygons(
        data = data$base,
        color = 'darkblue',
        weight = 1,
        fillOpacity = 0.5,
        popup = ~paste("Return Period:", rp, "<br>",
                       "Scenario:", scenario, "<br>",
                       "Flood Area (km²):", Area_km)
      ) %>% 
      addPolygons(
        data = data$restoration,  # Restoration scenario data
        color = 'yellow',  # Use palette for restoration scenario
        weight = 1,
        fillOpacity = 0.6,
        group = "Restoration Scenario",  # Add to a group for layer control
        popup = ~paste("Return Period:", rp, "<br>",
                       "Scenario:", scenario, "<br>",
                       "Flood Area (km²):", Area_km)
      )
  })
}