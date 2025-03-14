# load packages ----
library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(leaflet)

library(shinycssloaders)
library(markdown)
library(fresh)
library(sf)
library(dplyr)
library(rsconnect)
library(DT)
library(readr)
library(tidymodels)
library(shinydashboard)

#........Load shapefiles..........
# Define inputs
return_periods <- c("10", "50", "100", "500")
scenarios <- c("base", "ecological_25", "structural_05", "structural_25")

data_folder <- "../raw-data/PuertoRico_Current_Restored"

load_shapefile <- function(return_period, scenario, location_folder, location) {
  file_path <- file.path("..", "raw-data", location_folder, paste0(location, "_rp", return_period, "_", scenario, ".shp"))
  
  shapefile <- st_read(file_path, quiet = TRUE)
  
  if (!is.null(st_crs(shapefile)) && st_crs(shapefile)$epsg != 4326) {
    shapefile <- st_transform(shapefile, crs = 4326)
  }
}
#.........Load EJScreen shapefile...............
ej_flood <- st_read("../raw-data/clean/ej_flood.gpkg")
ej_flood_df <- st_drop_geometry(ej_flood)
ej_flood_df$STATE_NAME <- factor(ej_flood_df$STATE_NAME)
#.........Load Tabular Data..........
#hurricane data
data_folder2 <- "../raw-data"
hurricane_file_path <- file.path(data_folder2, "hurricane_loss_data.csv")
hurricane_data <- read.csv(hurricane_file_path)

#coral reef benefit data
benefit_file_path <- file.path(data_folder2, "reef_benefits.csv")
reef_benefits <- read.csv(benefit_file_path)
 
reef_clean <- pivot_longer(reef_benefits, cols = c("buildings", "economic", "total_value"), names_to = "value_type", values_to = "value_usd")
  


#............custom ggplot theme (apply to both plots)...........
myCustomTheme <- function() {
  theme_light() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.position = "bottom",
      panel.border = element_rect(linewidth = 0.7)
    )
}