library(mapboxapi) 
library(ggplot2) 
library(patchwork) 
library(tidyverse)
 
nationals_park <-   "1500 S capitol st SE, Washington, DC 20003"
midnight <- mb_isochrone(
  nationals_park, 
  time = 10,
  depart_at = "2022-03-31T09:00"
)

rush hour <- mb_isochrone( 
  nationals_park, 
  time = 10,
  depart_at = "2022-04-11T00:00"
)

dc_basemap <- layer_static_mapbox(
  location = midnight, 
  buffer dist 
= 500, 
style_id 
- "streets-vll", 
"mapbox" , 
username - 
mb_geocode(nationals_park, 
overlay_sf 
midnight_map 
ggplot() + 
dc_basemap 
geom_sf(data 
midnight, color - 
"navy" 
labs(title 
theme_void ( ) 
rush_hour_map 
dc_basemap 
geom_sf(data - 
labs(title 
theme_void ( ) 
midnight_map 
Washington , 
DC 20003" 
  output 
  fill 
  "sf") 
"10 minute drive at + 
ggplot() + 
rush hour, color - 
"navy" , 
fill 
"10 minute drive at 5:00pm") + 
rush_hour_map 
