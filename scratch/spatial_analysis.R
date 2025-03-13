# ---- Clean Raw Data ----
# Load EJ data
gdb_path <- "~/Bren/244 Advance data/shiny_riskreduction/raw-data/EJScreen_2024_Tract_StatePct_with_AS_CNMI_GU_VI.gdb"
layer_EJScreen <- st_layers(dsn = gdb_path)
ej_data <- st_read(dsn=gdb_path, layer = "EJSCREEN_StatePct_with_AS_CNMI_GU_VI")

# Filter for florida and pr
ej_sf <- ej_data %>% 
  filter(STATE_NAME %in% c("FLORIDA", "PUERTO RICO"))
st_write(ej_sf, "~/Bren/244 Advance data/shiny_riskreduction/raw-data/clean/ej_filtered.shp")
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
ej_projected$flood <- ifelse(lengths(st_intersects(ej_projected, flood_sf)) > 0, 1, 0)

# Save to directory
st_write(ej_projected, "~/Bren/244 Advance data/shiny_riskreduction/raw-data/clean/ej_flood.shp")

# ---- Perform Logistic classification ----
# Select features
data <- ej_projected %>% 
  select(STATE_NAME, UNEMPPCT, LOWINCPCT, UNDER5PCT, OVER64PCT, AREAWATER, DEMOGIDX_2, flood)
data <- st_drop_geometry(data)
data$flood <- factor(data$flood)
data$STATE_NAME <- factor(data$STATE_NAME)

# Balance the dataset (optional)
#data <- ovun.sample(flood ~ UNEMPPCT + LOWINCPCT + UNDER5PCT, data = data, method = "both")$data

# Scale numeric predictors
data <- data %>%
  mutate(across(where(is.numeric), scale))
# Define model 
log_md <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

# Recipe
recipe <- recipe(flood ~ ., data=data)

# Workflow
log_workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(log_md)

# Fit model on entire dateset
log_fit <- fit(log_workflow, data=data)

# ---- Summarize Results ----
summary(log_fit)
result <- log_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()
result
# A Demographic Index is based on the average of two demographic indicators; Percent Low-Income and Percent Minority.
# ---- proper analysis steps ----
# Check needs for imputation 
# Check class imbalance
# Split data 
# Train model 
# Set engine
# Create recipe
# Fit model
# Extract results

# CLassify with Model
