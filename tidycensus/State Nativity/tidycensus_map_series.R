library(tidyverse)
library(tidycensus)
library(sf)
library(crsuggest)
options(tigris_use_cache = TRUE)

walk(state.name, ~{
  in_state <- get_acs(
    geography = "tract",
    variables = "B05002_003",
    summary_var = "B05002_001",
    state = .x,
    geometry = TRUE
  ) %>%
    mutate(percent = 100 * (estimate / summary_est)) 
  
  state_crs <- suggest_top_crs(in_state, units = "m",
                               inherit_gcs = FALSE)
  
  in_state_proj <- st_transform(in_state, state_crs) %>%
    filter(GEOID != "15003981200") # for Hawaii archipelago
  
  gg <- ggplot(in_state_proj, aes(fill = percent)) + 
    geom_sf(color = NA) + 
    theme_void(base_family = "Roboto", base_size = 14) +
    labs(title = glue::glue("Percent of residents born in {.x}"),
         subtitle = "2016-2020 ACS",
         caption = "@kyle_e_walker | tidycensus R package",
         fill = "") + 
    scale_fill_viridis_c(option = "rocket", direction = -1,
                         labels = scales::label_percent(scale = 1)) + 
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  ggsave(glue::glue("img/born_in_{.x}.png"), gg, dpi = 600, bg = "white",
         height = 7, width = 7)  
})