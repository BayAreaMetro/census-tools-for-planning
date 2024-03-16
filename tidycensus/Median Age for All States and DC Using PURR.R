library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)

state_names <- c(state.name, "District of Columbia")
names(state_names) <- state_names

tictoc::tic()
age_maps <- map(state_names, ~{
  
  age_data <- get_acs(
    geography = "tract",
    variables = "B01002_001",
    state = .x,
    geometry = TRUE
  )
  
  ggplot(age_data, aes(fill = estimate)) + 
    geom_sf(color = NA) + 
    theme_void() + 
    scale_fill_viridis_c(option = "magma") + 
    labs(title = .x,
         subtitle = "Median age, 2017-2021 ACS",
         fill = "Estimate")
})
tictoc::toc()
