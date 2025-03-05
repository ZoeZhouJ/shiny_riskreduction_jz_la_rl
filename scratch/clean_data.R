# Run in console with raw data
# Define the directory for Florida files
florida_dir <- "~/Bren/244 Advance data/shiny_riskreduction/raw-data/Florida_Current_Restored"

# List all shapefiles and associated files in the directory
florida_files <- list.files(florida_dir, full.names = TRUE)

# Define a mapping of old scenario names to new names
rename_map <- list(
  "ecological_25" = "ecological",
  "structural_125" = "structural_25",
  "structural_25_w5" = "structural_05"
)

# Loop through all files and rename them
for (file in florida_files) {
  # Extract the base name of the file
  base_name <- basename(file)
  
  # Check if the file matches any of the old scenario names
  for (old_name in names(rename_map)) {
    if (grepl(old_name, base_name)) {
      # Replace the old name with the new name
      new_name <- gsub(old_name, rename_map[[old_name]], base_name)
      
      # Construct the full new file path
      new_file <- file.path(florida_dir, new_name)
      
      # Rename the file
      file.rename(file, new_file)
      cat("Renamed:", file, "->", new_file, "\n")
    }
  }
}
