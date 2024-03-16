library (tidycensus)
library (tidyverse)
library(mapview)
library(tigris)

options(tigris_use_cache=TRUE)

puma_income_22 <- get_acs(
  geography = "public use microdata area",
  variables = "B19013_001",
  geometry = TRUE,
  cb = FALSE,
  state = "CA",
  year = 2022,
  survey = "acs1"
  )

mapview(puma_income_22,
        zcol="estimate",
        layer.name = "Median household income")
