library(tigris)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)

library(showtext)
showtext_auto()
font_add_google("Montserrat")

all_places <- places(cb = TRUE)

top_names <- all_places %>%
  sf::st_drop_geometry() %>%
  group_by(NAME) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  slice_max(order_by = n, n = 20)

ggplot(top_names, aes(x = n, y = reorder(NAME, n))) +
  geom_col(width = 0.1, color = "#1a730f", fill = "#1a730f") +
  geom_point(size = 3, color = "#1a730f", fill = "#1a730f") +
  theme_minimal(base_size = 16, base_family = "Montserrat") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) + 
  labs(title = "Most common names for\nUS Census-designated places",
       caption = "@kyle_e_walker | tigris R package",
       x = "Number of places",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


us_boundaries <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

place_names <- top_names$NAME
names(place_names) <- place_names

place_maps <- map(place_names, function(name) {
  overlay <- all_places %>%
    filter(NAME == name) %>%
    shift_geometry() %>%
    st_centroid()
  
  ggplot() +
    geom_sf(data = us_boundaries, fill = "white", color = "grey50") +
    geom_sf(data = overlay, color = "#1a730f", size = 2.5,
            alpha = 0.85) +
    theme_void(base_size = 16, base_family = "Montserrat") +
    labs(title = glue::glue('Locations of US Census-designated places named "{name}"'),
         caption = "@kyle_e_walker | tigris R package")
})
