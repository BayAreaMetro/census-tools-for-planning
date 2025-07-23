# Export_2020_ZCTA_Shapefile.r

# Load required libraries
library(tidycensus)
library(tidyverse)
library(sf)

# Download ZCTA shapefile (ZIP Code Tabulation Areas)
# Set `cb = TRUE` to use the generalized cartographic boundary files
zcta_sf <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population (a single variable is needed at minimum, removed below)
  geometry = TRUE,
  year = 2020,
  cb = TRUE
)

# Remove attributes and only keep the geometry for later use
zcta_geom <- zcta_sf %>%
  select(GEOID, geometry) 

# Define output path
output_path <- "M:/Data/Census/Geography/2020_ZCTAs"

# Export to shapefile (folder will contain multiple files)
st_write(zcta_geom, dsn = output_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
