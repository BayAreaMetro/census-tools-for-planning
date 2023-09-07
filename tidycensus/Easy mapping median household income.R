tidycensus::get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CA",
  geometry = TRUE
) |>
  mapview::mapview(
    zcol = "estimate",
    layer.name = "Median household income"
  )
