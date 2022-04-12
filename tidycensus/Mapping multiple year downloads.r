# Mapping multiple year downloads.r

library(tidycensus)
library(tidyverse)

years <- 2005:2019 %>% 
  set_names(2005:2019)

df <- years %>% 
  map_dfr(~get_acs(geography = "county",
                  variables = "B19013_001",
                  state="NY",
                  county="New York",
                  survey="acs1",
                  year = .x),
          .id="year")
