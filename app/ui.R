ui <- navbarPage(
  title="Coastal Risk Reduction Data Explorer",
  theme = bs_theme(bootswatch = 'flatly'),
  # Page 1 intro tabPanel ----
  tabPanel(title = "About this App",
           sidebarLayout(
             sidebarPanel(
               h4("Explore the App"),
               p("This app provides interactive visualizations and insights into the impacts of hurricanes, flood projections, 
         and the benefits of coral restoration. It is designed to help researchers, policymakers, and stakeholders 
         understand the relationships between natural disasters, coral reef health, and restoration benefits."),
         p("The app is divided into three main pages:"),
         tags$ul(
           tags$li("Page 1: Summary of Project and Data Sources - Introduction to project, data sources, and project exploration."),
           tags$li("Page 2: Hurricane Impact Explorer - Explore the economic and social impacts of hurricanes."),
           tags$li("Page 3: Flood Extent Projection - Visualize flood extent maps based on coral cover and storm return intervals."),
         )
             ),#END sidebarPanel
         mainPanel(
           h3("Summary"),
           p("Coastal communities worldwide are increasingly vulnerable to hurricanes, coastal flooding, and sea-level rise, leading to severe economic, social, and environmental consequences. 
             Natural solutions, such as coral reef restoration, are gaining attention as cost-effective measures to mitigate these risks while promoting ecological resilience.
             This project explores how coral reef restoration can provide both economic and social benefits by reducing storm-related flooding, protecting infrastructure, and enhancing coastal resilience."),
           h3("Data Sources"),
           p("This app uses data from the following authoritative sources:"),
           tags$ol(
             tags$li(
               strong("Flood Extent Projection:"),
               " USGS Data Release - ",
               a("Flood extent projection", href = "https://www.sciencebase.gov/catalog/item/5f2c759782ceae4cb3c2d0ab", target = "_blank"),
               ". This dataset provides flood extent polygons for Florida and Puerto Rico based on wave-driven total water levels."
             ),
             tags$li(
               strong("Hurricane Impact:"),
               " USGS Report - ",
               a("Hurricane Impact Report", href = "https://pubs.usgs.gov/of/2021/1056/ofr20211056.pdf", target = "_blank"),
               ". This report details the economic and social impacts of hurricanes, focusing on damages to infrastructure and economic activity."
             ),
             tags$li(
               strong("Coral Restoration Benefit:"),
               " Science Advances - ",
               a("Restoration Benefit Study", href = "https://www.science.org/doi/10.1126/sciadv.adn4004", target = "_blank"),
               ". This study quantifies the economic and social benefits of coral restoration, including flood risk reduction and coastal resilience."
             )
           ),
           h3("Acknowledgments"),
           p("This app is built using publicly available datasets and reports from the USGS and Science Advances. 
         It aims to provide actionable insights for disaster management, coastal protection, and environmental restoration."),
         img(src="https://eos.org/wp-content/uploads/2023/11/hurricane-lee.jpg", width = 800)
         )
           )
         
  ), # END (Page 1) intro tabPanel
  
  # (Page 2) hurricane impact data viz tabPanel ----
  tabPanel(title = "Coastal Impact Explorer",
           # tabsetPanel to contain tabs for data viz ----
           tabsetPanel(
             # Impact tabPanel ----
             tabPanel(title = "Hurricane Impact",
                      
                      # Impact sidebarLayout ----
                      #"placeholder: impact data description here! (histogram)",
                      sidebarLayout(
                        
                        # Impact sidebarPanel ----
                        sidebarPanel(
                          #"placeholder: impact histogram inputs here",
                          # channel type pickerinput ---- location
                          pickerInput(inputId = "location", 
                                      label = "Select Location:", 
                                      choices = unique(hurricane_data$sublocation), #c("Florida", "Puerto Rico", "Both"), 
                                      #selected = unique(hurricane_data$sublocation), 
                                      selected = unique(hurricane_data$sublocation)[!unique(hurricane_data$sublocation) %in% c("Palm Beach", "Martin", "Broward")],
                                      options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                      multiple = TRUE
                              ),
                        
                          
                          # PickerInput for return_interval
                          pickerInput("return_interval", "Select Return Interval:", 
                                      choices = c("10", "50", "100", "500"), 
                                      selected = "500", 
                                      options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                      multiple = FALSE)
                        ), # END Impact sidebarPanel
                        
                        # Impact mainPanel ----
                        mainPanel(
                          #"placeholder: impact histograms go here",
                          # impact histogram output
                          plotOutput(outputId = "impact_histogram"),
                          tableOutput(outputId = "ANOVA_output")
                        ), # END Impact mainPanel
                      ), # END impact sidebarLayout
             ), # END Impact tabPanel
             
             # Benefit tabPanel ----
             tabPanel(title = "Benefit",
                      
                      # Benefit sidebarLayout ----
                      #"placeholder: Coral description/instruction here! (histograms)",
                      sidebarLayout(
                        
                        # Benefit sidebarPanel ----
                        sidebarPanel(
                          "placeholder: Benefit histogram inputs here",
                          # channel type pickerinput ----
                          # pickerInput(inputId = "benefit_input",
                          #             label = "Select types of benefit:",
                          #             choices = unique(df$columns_names),
                          #             #selected = c("Social", "Building"),
                          #             multiple = TRUE,
                          #             options = pickerOptions(actionBox = TRUE)
                          # ) # END pickerInput
                        ), # END Benefit sidebarPanel
                        
                        # Benefit mainPanel ----
                        mainPanel(
                          "placeholder: Benefit histograms go here",
                          # Benefit histogram output
                          plotOutput(outputId = "benefit_histogram_output")
                        ), # END Benefit mainPanel
                      ), # END Benefit sidebarLayout
             ) # END Benefit tabPanel
           ), # End tabsetPanel 
           
  ), # END (Page 2) hurricane impact data viz tabPanel
  
  # (Page 3) map panel ----
  tabPanel(title = "Flood Extent Projection",
           # flood sidebarLayout ----
           sidebarLayout(
             # flood sidebarPanel
             sidebarPanel(
               # Location selector
               selectInput(inputId = "location_input",
                           label = "Select Location:",
                           choices = c("Florida", "Puerto Rico"),
                           selected = "Puerto Rico",
                           multiple = FALSE
                           ),
               
               
               selectInput(inputId = "zone_input",
                           label = "Select Zone:",
                           choices = NULL,
                           selected = NULL
               ),
               
               pickerInput(inputId = "scenario_input",
                           label = "Select Restoration Scenario:",
                           choices = scenarios,
                           selected = "base",
                           #multiple = TRUE,
                           options = pickerOptions(actionsBox = TRUE)),
               #helpText("Choose from three coastal restoration scenarios: 'ecological_25' with 0.25m-high corals on a 25m-wide reef, 'structural_25' with a 1m-high structure and 0.25m corals on a 25m-wide reef, or 'structural_05' with a 1m-high structure and 0.25m corals on a 5m-wide reef. Default scenario is the baseline condition. Select to compare different coastal management strategies."),
               helpText(
                 tags$div(
                   tags$p("Choose from three coastal restoration approaches:"),
                   tags$ul(
                     tags$li(tags$strong("Ecological_25:"), "0.25m-high corals on 25m-wide reef"),
                     tags$li(tags$strong("Structural_25:"), "1m-high structure with 0.25m corals on 25m-wide reef"),
                     tags$li(tags$strong("Structual_05:"), "1m-high structure with 0.25m corals on 5m-wide reef")
                   ),
                   tags$p("Default scenario provides baseline comparison.")
                 )
               ),
               radioButtons(inputId = "return_interval_input",
                            label = "Select Return Period (years):",
                            choices = return_periods,
                            selected = "500"),
               
               
               
               helpText(
                 tags$div(
                   tags$p("Choose the hydrological return period representing average time between events: Longer periods indicate rarer, more extreme events. Default is 500-year interval. Select to define event probability.")
                 )
               )
               #helpText("Choose the hydrological return period for your analysis. The return period represents the average time interval between events of a specific magnitude. Select from available options, with a default of 500 years. Longer periods indicate rarer events.")
             ), # END flood sidebarPanel
             # flood mainPanel
             mainPanel(
               
               # flood map output
               # leaflet box ----
               box(width = 30,
                  height = 600,
                   title = tags$strong("Map of Flood Extent Projection"),
                   
                   # leaflet output ----
                   leafletOutput(outputId = "flood_map_output",
                                 width = "100%", 
                                 height = "650") |> 
                     withSpinner(type = 1, color = "#18bc9c"),
                   
               ), # END leaflet box
               
             ) #END flood mainPanel
           ) # END flood sidebarLayout
           
  ) # END (Page 3) map panel
  
) # END ui

