library(tidyverse)
library(palmerpenguins)
library(shiny)

### Create the userinterface
# Checkout shiny widget gallery
ui <- fluidPage(
  titlePanel('title here: '),
  sidebarLayout(
    sidebarPanel('put my widgets here:',
                 radioButtons(
                   inputId = 'periods', #variable column name
                   label = 'Choose wave-energy return periods',
                   choices = c('10-yr wave-energy return period'='10-','50-yr wave-energy return period'='50-','100-yr wave-energy return period'='100-','500-yr wave-energy return period'='500-')
                 ),
                 radioButtons(
                   inputId = 'penguin_species',
                   label = 'Choose penguin species',
                   choices = c('Adlie', 'Gentoo', 'Chinstrap')
                 ),
                 selectInput(inputId = 'pt_color',
                             label='Select point color',
                             choices = c('Roses'='red',
                                         'violets'='purple',
                                         'oranges'='orange'))
                 ),
    mainPanel('graph here:',
              # plotOutput(outputId = 'flood_map'),
              plotOutput(outputId = 'penguin_plot'),
              h3('Summary table'),
              tableOutput(outputId='penguin_table'))
  )
)

### Create the server function
server <- function(input, output){
  # make a reactive function responde to the ui 
  # flood selection
  flood_select <- reactive({
    flood_df <- flood %>% 
      filter(periods==input$periods)
  })
  
  output$flood_map <- renderPlot({
    ggplot(data = flood_select())+
      geom_point(aes(x=latitude, y=longtitude),
                 color=input$pt_color)
  })
  
  # penguin examples
  penguin_select <- reactive({
    penguins_df <- penguins %>% 
      filter(species==input$penguin_species)
  })
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select())+
      geom_point(aes(x=flipper_length_mm, y=body_mass_g),
                 color=input$pt_color)
  })
  penguin_sum_table <- reactive({
    penguin_summary_df <- penguins %>% 
      filter(species==input$penguin_species) %>% 
      group_by(sex) %>% 
      summarize(mean_flop=mean(flipper_length_mm, na.rm=TRUE),
                mean_mass=mean(body_mass_g, na.rm=TRUE))
  })
  output$penguin_table <- renderTable({
    penguin_sum_table()
  })
}

### Combine them into an app
shinyApp(ui=ui, server=server)