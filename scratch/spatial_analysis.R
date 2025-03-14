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
st_write(ej_projected, "~/Bren/244 Advance data/shiny_riskreduction/raw-data/clean/ej_flood.gpkg", append=FALSE)
ej_flood <- st_read("~/Bren/244 Advance data/shiny_riskreduction/raw-data/clean/ej_flood.gpkg")

# ---- Perform Logistic classification ----
# Select features
data <- st_drop_geometry(data)

data <- ej_flood %>% #ej_projected %>% 
  select(STATE_NAME, D2_DSLPM, D2_RSEI_AIR, D2_PTRAF, D2_PTSDF, D2_PWDIS, D2_NO2, DEMOGIDX_2, flood)
data <- st_drop_geometry(data)
data$flood <- factor(data$flood)
#data$STATE_NAME <- factor(data$STATE_NAME)
data <- data %>% 
  filter(STATE_NAME == "PUERTO RICO")
data <- data %>% select(-STATE_NAME)
# Remove predictors with all missing values
#data <- data %>% select_if(~ !all(is.na(.)))
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

# ---- evaluation ----

predicted_probs <- predict(log_fit, new_data = data, type = "prob")
predicted_classes <- predict(log_fit, new_data = data, type = "class")
data_pred <- data %>% 
  mutate(pred_prob = predicted_probs$.pred_1) %>% 
  mutate(pred_class = predicted_classes$.pred_class)

accuracy(data_pred, truth = flood, estimate = pred_class)

ggplot(data_pred, aes(x = pred_prob, fill = flood)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Predicted Probabilities",
    x = "Predicted Probability",
    y = "Density"
  ) +
  theme_minimal()


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
