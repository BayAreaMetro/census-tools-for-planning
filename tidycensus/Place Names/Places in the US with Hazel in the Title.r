library(tigris)
library(tidyverse)
library(showtext)
library(sf)

options(tigris_use_cache = TRUE)
font_add_google("Roboto", "Roboto")
showtext_auto()

state_borders <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

point_places <- places(cb = TRUE) %>%
  filter(str_detect(NAME, "Hazel")) %>%
  st_centroid() %>%
  shift_geometry()

ggplot() + 
  geom_sf(data = state_borders, fill = "grey80", color = "grey20") + 
  geom_sf(data = point_places, color = "navy", alpha = 0.75) + 
  theme_void(base_size = 16, base_family = "Roboto") + 
  labs(title = "U.S. Census-designated places with 'Point(e)' or 'Punta' in their name", 
       subtitle = "Source: Census cartographic boundary shapefiles",
       caption = "@kyle_e_walker | tigris R package")
