library(tidycensus)
library(tidyverse)
library(sf)
library(segregation)
library(mapboxapi)
library(tigris)

mapbox_token <- readLines ("H:/Mapbox/Mapbox.txt")

# Add token
mb_access_token(mapbox_token,install = TRUE)

ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "CA",
  geometry = TRUE,
  year = 2019
) 

us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", CA Urbanized Area (2010)")))

ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

la_entropy <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
    data = .x,
    group = "variable",
    weight = "estimate",
    base = 4)))

la_entropy_geo <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_entropy, by = "GEOID")

la_city_hall <- mb_geocode("City Hall, Los Angeles CA")

minutes_to_downtown <- mb_matrix(la_entropy_geo, la_city_hall)

la_entropy_geo$minutes <- as.numeric(minutes_to_downtown)

ggplot(la_entropy_geo, aes(x = minutes_to_downtown, y = entropy)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 80)) + 
  labs(title = "Diversity gradient, Los Angeles urbanized area",
       x = "Travel-time to downtown Los Angeles in minutes, Census tracts",
       y = "Entropy index")
