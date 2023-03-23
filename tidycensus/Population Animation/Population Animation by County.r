library(tigris)
library(tidycensus)
library(tidyverse)
library(gganimate)
library(scales)
options(tigris_use_cache = TRUE)

us_county_pop <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry() %>%
  mutate(rank = factor(round(min_rank(desc(value)), -2)))

anim <- ggplot(us_county_pop, aes(fill = value)) +
  geom_sf(linewidth = 0.1) +
  theme_void(base_family = "Roboto", base_size = 12) +
  scale_fill_viridis_c(option = "cividis", trans = log10_trans(), labels = scales::label_number_si()) +
  labs(fill = "Value",
       title = "US County Populations, 2020",
       subtitle = "@kyle_e_walker | tidycensus / gganimate R packages",
       caption = "") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.5, "cm")) +
  transition_states(rank, state_length = 1) +
  shadow_mark()

animate(anim, height = 550, width = 800, end_pause = 20)