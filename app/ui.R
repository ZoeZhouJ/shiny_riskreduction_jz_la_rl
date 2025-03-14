ui <- dashboardPage(
  
  skin = "blue",  # Theme color
  dashboardHeader(title = "Coastal Risk Reduction Explorer", titleWidth = 350),
  
  
  dashboardSidebar(width = 300,
                   
                   sidebarMenu(
                     HTML(paste0(
                       "<br>",
                       "<a href='www/Logo-maker-project-1.png' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='Logo-maker-project-1.png' width='250'></a>",
                       "<br>",
                       "<p style = 'text-align: center;'><small><a target='_blank'>ResiliReefs logo disclaimer</a></small></p>",
                       "<br>"
                     )),
                     
                     
                     
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     menuItem("Hurricane Impact Explorer", tabName = "impact", icon = icon("wind")),
                     menuItem("Flood Extent Projection", tabName = "flood", icon = icon("water")),
                     menuItem("Data Sources", tabName = "data", icon = icon("database")),
                     
                     # Social media icons
                     HTML(paste0(
                       "<br><br>",
                       "<table style='margin-left:auto; margin-right:auto;'>",
                       "<tr>",
                       "<td style='padding: 5px;'><a href='https://www.instagram.com/resilireefs' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                       "</tr>",
                       "</table>",
                       "<br>"
                     )),
                     
                     HTML(paste0(
                       "<p style = 'text-align: center;'><small>&copy; ResiliReefs - 2025</small></p>"
                     ))
                   ) # End sidebarMenu
  ), # End dashboardSidebar
  
  dashboardBody(
    
    # Hide error messages in UI
    tags$style(type="text/css", 
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tabItems(
      
      # Home Page ----
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Coastal Risk Reduction Data Explorer"),
                
                tags$p(
                  tags$small("By: Letty Aguilar, Roxana Lagunas, Zoe Zhou"),
                  style="color: gray; font-size: 15px; text-align: left;"
                ),
                
                mainPanel(
                  h3("Summary"),
                  p("Coastal communities worldwide are increasingly vulnerable to hurricanes, coastal flooding, and sea-level rise, leading to severe economic, social, and environmental consequences. 
                     Natural solutions, such as coral reef restoration, are gaining attention as cost-effective measures to mitigate these risks while promoting ecological resilience.
                     This project explores how coral reef restoration can provide both economic and social benefits by reducing storm-related flooding, protecting infrastructure, and enhancing coastal resilience."),
                  
                  h3("Exploring the App"),
                  p("This app provides interactive visualizations and insights into the impacts of hurricanes, flood projections, 
                     and the benefits of coral restoration. It is designed to help researchers, policymakers, and stakeholders 
                     understand the relationships between natural disasters, coral reef health, and restoration benefits."),
                  
                  p("The app is divided into three main pages:"),
                  tags$ul(
                    tags$li("Page 1: Summary of Project and Data Sources - Introduction to project, data sources, and project exploration."),
                    tags$li("Page 2: Hurricane Impact Explorer - Explore the economic and social impacts of hurricanes."),
                    tags$li("Page 3: Flood Extent Projection - Visualize flood extent maps based on coral cover and storm return intervals.")
                  ), 
                  
                  img(src="https://eos.org/wp-content/uploads/2023/11/hurricane-lee.jpg", width="800px")
                  
                ) # END mainPanel 
              ) # END fluidPage
      ), # END Home Page
      
      
      # (Page 2) Hurricane Impact Data Visualization ----
      tabItem(tabName = "impact",
              
              fluidPage(
                titlePanel("Coastal Impact Explorer"),
                
                # Tabset for different impact visualizations
                tabsetPanel(
                  
                  # Hurricane Impact Tab ----
                  tabPanel(title = "Hurricane Impact",
                           
                           sidebarLayout(
                             sidebarPanel(
                               
                               # Location selector
                               pickerInput(inputId = "location", 
                                           label = "Select Location:", 
                                           choices = unique(hurricane_data$sublocation),  
                                           selected = unique(hurricane_data$sublocation)[!unique(hurricane_data$sublocation) %in% c("Palm Beach", "Martin", "Broward")],
                                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                           multiple = TRUE
                               ),
                               
                               # Return Interval selector
                               pickerInput("return_interval", "Select Return Interval:", 
                                           choices = c("10", "50", "100", "500"), 
                                           selected = "500", 
                                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                           multiple = FALSE)
                             ), # END sidebarPanel
                             
                             mainPanel(
                               fluidRow(
                                 box(
                                   width = 12,
                                   title = tags$strong("Hurricane Impact Visualization"),
                                   
                                   # Histogram Output
                                   plotOutput(outputId = "impact_histogram") %>% 
                                     withSpinner(type = 1, color = "#18bc9c")
                                 ) # END box
                               ) # END fluidRow
                             ) # END mainPanel
                           ) # END sidebarLayout
                  ), # END Hurricane Impact Tab
                  
                  # Benefit Tab ----
                  tabPanel(title = "Benefit",
                           
                           sidebarLayout(
                             sidebarPanel(
                               pickerInput(inputId = "location", 
                                           label = "Select Location:", 
                                           choices = unique(reef_clean$sublocation), #c("Florida", "Puerto Rico", "Both"), 
                                           #selected = unique(hurricane_data$sublocation), 
                                           selected = unique(reef_clean$sublocation),
                                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                           multiple = TRUE
                               ), #END Pickerinput -- location
                               
      
                               
                               #pickerInput --- benefit category
                               pickerInput("value_type", "Select Benefit Category:", 
                                           choices = unique(reef_clean$value_type), 
                                           selected = unique(reef_clean$value_type),
                                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                           multiple = TRUE
                               ), #END Pickerinput -- benefit category
                               
  
                             ), # END sidebarPanel
                             
                             mainPanel(
                               fluidRow(
                                 box(
                                   width = 12,
                                   title = tags$strong("Benefit Analysis"),
                                   
                                   # Benefit Histogram Output
                                   plotOutput(outputId = "benefit_histogram_output") %>% 
                                     withSpinner(type = 1, color = "#18bc9c")
                                 ) # END box
                               ) # END fluidRow
                             ) # END mainPanel
                           ) # END sidebarLayout
                  ) # END Benefit Tab
                ) # END tabsetPanel
              ) # END fluidPage
      ), # END tabItem (Hurricane Impact)
      
      
      # Flood Projection ----
      tabItem(tabName = "flood",
              fluidPage(
                titlePanel("Flood Extent Projection"),
                
                sidebarLayout(
                  sidebarPanel(
                    
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
                                choices = c("base", "ecological_25", "structural_25", "structural_05"),  # Ensure correct choices
                                selected = "base",
                                options = pickerOptions(actionsBox = TRUE)
                    ),
                    
                    helpText(
                      tags$div(
                        tags$p("Choose from three coastal restoration approaches:"),
                        tags$ul(
                          tags$li(tags$strong("Ecological_25:"), "0.25m-high corals on 25m-wide reef"),
                          tags$li(tags$strong("Structural_25:"), "1m-high structure with 0.25m corals on 25m-wide reef"),
                          tags$li(tags$strong("Structural_05:"), "1m-high structure with 0.25m corals on 5m-wide reef")
                        ),
                        tags$p("Default scenario provides baseline comparison.")
                      )
                    ),
                    
                    radioButtons(inputId = "return_interval_input",
                                 label = "Select Return Period (years):",
                                 choices = c("10", "50", "100", "500"),  # Ensure correct choices
                                 selected = "500"
                    ),
                    
                    helpText(
                      tags$div(
                        tags$p("Choose the hydrological return period representing average time between events. 
                   Longer periods indicate rarer, more extreme events. Default is 500-year interval.")
                      )
                    )
                  ), # END sidebarPanel
                  
                  # Main Panel for the Map Output
                  mainPanel(
                    fluidRow(
                      box(
                        width = 12,  # Adjusted width to fit layout properly
                        title = tags$strong("Map of Flood Extent Projection"),
                        
                        # Leaflet Map Output
                        leafletOutput(outputId = "flood_map_output",
                                      width = "100%", 
                                      height = "650") %>% 
                          withSpinner(type = 1, color = "#18bc9c")  # Loading spinner
                      ) # END box
                    ), # END fluidRow
                    
                    # Add fluidRow for reactive table
                    fluidRow(
                      column( 
                        width = 12,
                        
                        selectInput(inputId = 'state_input',
                                    label = 'Select State to Study Environmental Justice',
                                    choices = c("FLORIDA", "PUERTO RICO"),
                                    selected = "PUERTO RICO",
                                    multiple = FALSE
                                    )
                      )
                    ), # END fluidRow for reactive table input
                    
                    # FluidRow for reactive table output
                    fluidRow(
                      box(
                        width = 12,
                        title = tags$strong("Logistic Regression Model Coefficients"),
                        
                        # Table Output Widget for Model Results
                        DT::dataTableOutput(outputId = "table_output") %>% 
                          withSpinner(type = 1, color = "#18bc9c")  # Loading spinner
                      ) # END box
                    ) # END fluidRow for reactive table output
                  
                  ) # END mainPanel
                ) # END sidebarLayout
              ) # END fluidPage
      ), # END tabItem(flood)
      
      
      # Data Sources ----
      tabItem(tabName = "data",
              fluidPage(
                titlePanel("Data Information and Sources"),
                mainPanel(
                  h3("Data Information"),
                  p("WILL INSERT TEXT AFTER SENT"),
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
                ),
                img(src="redseareef.avif", width="650px")
              ), # END 
              
      )
      
    ) # End tabItems
  ) # End dashboardBody
) # End dashboardPage







