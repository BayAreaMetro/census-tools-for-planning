# Bring in libraries

library(tigris)
suppressMessages(library(tidyverse))
library(sf)


# Fetch all CA 2020 counties
# Convert projection to NAD83 / UTM zone 10N (ESPG 26910)

bay_counties <- counties(
  state = "CA",
  year = 2020
) %>% 
  st_transform(.,crs = 26910) %>% 
  filter(GEOID %in% c("06001","06013","06041","06055","06075","06081","06085","06095","06097")) %>% 
  select(GEOID,NAME,geometry)

# Export shapefile

st_write(bay_counties,file.path("M:\\Data\\GIS layers\\Counties","bay_counties.shp"))
