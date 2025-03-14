server <- function(input, output, session){
  # update zone choices based on selected location
  shiny::observe({
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

  })
  
  # Observe changes in the selected data and update the map
  shiny::observe({
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
  
  # Impact page output
  hurricane_filtered <- reactive({
      hurricane_df <- hurricane_data %>% 
        filter(sublocation %in% input$location) %>% 
        filter(return_interval == as.numeric(input$return_interval))
  })
  
  output$impact_histogram <- renderPlot({
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
  
  # Benefits page output
  reef_filtered <- reactive({
    reef_df <- reef_clean %>% 
      filter(sublocation %in% input$location)
  })
  
  output$benefit_histogram_output <- renderPlot({
    coral_benefit <- reef_filtered() # Get the filtered data
     
    
    # Check if the filtered data is empty before plotting
    if (nrow(coral_benefit) == 0) {
      return(NULL)  # If data is empty, don't plot
    }
    
    data_range <- range(coral_benefit$value_usd, na.rm = TRUE)
    binwidth <- (data_range[2] - data_range[1]) / 15
    
    ggplot(coral_benefit, aes(x= sublocation, y = value_usd, fill = value_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Histogram of Benefits from Coral Reef Restoration",
           x = "Locations",
           y = "Benefit Amount (in USD)") +
      scale_fill_manual(values = c("economic" = "turquoise", "buildings" = "grey", "total_value" = "forestgreen")) +
      myCustomTheme() +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust =1))
    })
  
  # ---- Page 3 reactive table ----
  # select input
  ej_state_df <- reactive({
    req(input$state_input)
    ej_state <- ej_flood_df[ej_flood_df$STATE_NAME == input$state_input, ]
    
    ej_state <- ej_state %>% 
      select(OVER64PCT, D2_DSLPM, D2_RSEI_AIR, D2_PTRAF, D2_PTSDF, D2_PWDIS, D2_NO2, DEMOGIDX_2, flood) %>% 
      rename(
        Over_Age64= OVER64PCT,
        Diesel_PM = D2_DSLPM,
      Air_Toxics = D2_RSEI_AIR,
      Traffic_Proximity = D2_PTRAF,
      Hazardous_Waste = D2_PTSDF,
      WasteWater_Discharge = D2_PWDIS,
      NO2_Concentration = D2_NO2,
      Demographic_Index = DEMOGIDX_2,
      Flood = flood
      )
    
    ej_state$Flood <- factor(ej_state$Flood)
    as.data.frame(ej_state)
  })
  
  # Reactive logistic model
  logistic_md <- reactive({
    data <- ej_state_df()

    # Define model
    log_md <- logistic_reg() %>%
      set_engine('glm') %>%
      set_mode('classification')
    
    # Recipe
    recipe <- recipe(Flood ~ ., data = data)
    
    # Workflow
    log_workflow <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(log_md)
    
    # Fit model
    log_fit <- fit(log_workflow, data = data)
    
    log_fit
    
  })
  # Render reactive table
  output$table_output <- DT::renderDataTable({
    log_fit <- logistic_md()
    # Extract and tidy results
    result <- log_fit %>% 
      extract_fit_parsnip() %>% 
      tidy() %>% 
      transmute(
        Term = term,
        `Odds Ratio` = round(exp(estimate), 3),
        `% Change in Odds of Flooding` = round((exp(estimate) - 1) * 100, 1), 
        `Std. Error` = round(std.error, 3),
        Statistic = round(statistic, 3),
        `P-Value` = scales::pvalue(p.value, accuracy = 0.001)
      )
    
  })
  
}