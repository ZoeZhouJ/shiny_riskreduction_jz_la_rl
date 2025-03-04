server <- function(input, output){
  # filter data with reactive({})
  selected_data <- reactive({
    req(input$return_interval_input, input$scenario_input) # Ensure inputs are available
    df <- load_shapefile(input$return_interval_input, input$scenario_input)
    print(df)
    return(df)
  })
  
  # plot with renderPlot
  output$flood_map_output <- renderLeaflet({ 
    leaflet() |> 
      addProviderTiles(providers$Esri.WorldStreetMap) |> 
      setView(-66.17723, 18.44559, zoom = 12) |>
      addMiniMap(toggleDisplay = TRUE,
                 minimized = FALSE)
      #addCircleMarkers(lng = -81.5, lat = 27.8, popup = "Florida") |>
      #addCircleMarkers(lng = -66.5, lat = 18.2, popup = "Puerto Rico")
  })
  
  # Observe changes in the selected data and update the map
  observe({
    data <- selected_data()
    leafletProxy("flood_map_output") %>%
      clearShapes() %>%
      addPolygons(
        data = data,
        color = ~flood_palette(input$scenario_input),
        weight = 1,
        fillOpacity = 0.5,
        popup = ~paste("Return Period:", rp, "<br>",
                       "Scenario:", scenario, "<br>",
                       "Flood Area (km²):", Area_km)
      )
  })
}