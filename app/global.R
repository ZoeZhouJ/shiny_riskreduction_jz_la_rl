# load packages ----
library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(fresh)
library(sf)
library(dplyr)

#........Load shapefiles..........
# Define available return periods and scenarios
return_periods <- c("10", "50", "100", "500")
scenarios <- c("base", "ecological_25", "structural_05", "structural_25")


data_folder <- "../raw-data/PuertoRico_Current_Restored"

load_shapefile <- function(return_period, scenario) {
  file_name <- paste0("PuertoRico_rp", return_period, "_", scenario, ".shp")
  file_path <- file.path(data_folder, file_name)
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  shapefile <- st_read(file_path, quiet = TRUE)
  
  if (!is.null(st_crs(shapefile)) && st_crs(shapefile)$epsg != 4326) {
    shapefile <- st_transform(shapefile, crs = 4326)
  }
}




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