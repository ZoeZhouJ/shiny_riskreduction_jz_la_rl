# Load EJ data
gdb_path <- "~/Bren/244 Advance data/shiny_riskreduction/raw-data/EJScreen_2024_Tract_StatePct_with_AS_CNMI_GU_VI.gdb"
layer_EJScreen <- st_layers(dsn = gdb_path)
ej_data <- st_read(dsn=gdb_path, layer = "EJSCREEN_StatePct_with_AS_CNMI_GU_VI")

# Filter for florida and pr
ej_sf <- ej_data %>% 
  filter(STATE_NAME %in% c("FLORIDA", "PUERTO RICO"))

# Load flood extent data 
florida_flood_fp <- "~/Bren/244 Advance data/shiny_riskreduction/raw-data/Florida_Current_Restored/Florida_rp50_base.shp"
pr_flood_fp <- "~/Bren/244 Advance data/shiny_riskreduction/raw-data/PuertoRico_Current_Restored/PuertoRico_rp50_base.shp"
florida_data <- st_read(florida_flood_fp)
pr_data <- st_read(pr_flood_fp)
pr_projected <- st_transform(pr_data, st_crs(florida_data))

# combine flood
flood_sf <- st_union(florida_data, pr_projected)

# Project EJ data to match flood shapefile
ej_projected <- st_transform(ej_sf, st_crs(florida_data))

# Perform spatial overlay
ej_projected$flood <- ifelse(length(st_intersects(ej_projected, flood_sf)) > 0, 1, 0)

# Save to directory
st_write(ej_projected, "~/Bren/244 Advance data/shiny_riskreduction/raw-data/clean/ej_flood.shp")

