##### Libraries to Load #####
library(tidyverse)
library(sf)
library(tidycensus)


##### Function parameters #####

# 1. geography_value = The geography that you wish to pull ACS data. Input one of these:
#                          - block group
#                          - tract
#                          - county
# 2. api_key_value   = Your Census API Key Value
# 3. variables_value = The ACS variable that you wish to work with
# 4. summation_type  = Do you want your final variable to be the mean of all variables in the hexagon, or the sum? You'd want the 
#                      sum for something like population (a count) and the mean for something like income (a rate). Input one of these:
#                          - sum
#                          - mean
# 5. state_value     = The state(s) that you wish to pull data from
# 6. county_value    = (OPTIONAL) The county(s) that you wish to pull data from within the above state
# 7. year_value      = What year of ACS data you wish to pull
# 8. survey_value    = What type of ACS data you wish to pull. Input one of these:
#                          - acs5
#                          - acs3
#                          - acs1
# 9. cell_size       = The length of the hexagon, measured from edge to opposite edge in MILES


##### Defining the function #####
make_hexagons_acs <- function(geography_value, api_key_value, variables_value, summation_type, state_value, county_value, year_value, survey_value, cell_size){
  
  ##### Checks to see if the inputs are correct #####
  
  #Checking if the required inputs are present
  if(
    base::missing(geography_value) |
    base::missing(api_key_value) |
    base::missing(variables_value) |
    base::missing(summation_type) |
    base::missing(state_value) |
    base::missing(year_value) |
    base::missing(cell_size)
  ){
    stop("You are missing one of the required inputs to this function")
  }
  
  #Check if the geography is county, tract, or block group
  if (!(geography_value == "county" | geography_value == "tract" | geography_value == "block group")){stop("Your geography type is not 'county', 'tract' or 'block group'")}
  
  #Checking if the number of variables is less than 1
  if (length(variables_value) > 1){stop("Please specify only one variable at a time")}
  
  #Check if the summation_type is sum or mean
  if (!(summation_type == "sum" | summation_type == "mean")){stop("Your summation type is not 'sum' or 'mean'")}
  
  
  #Check if the geography is county, tract, or block group
  if (!(survey_value == "acs5" | survey_value == "acs3" | survey_value == "acs1")){stop("Your survey type is not 'acs5', 'acs3' or 'acs1'")}
  
  
  ###### Creating an "overlap sum" function which will be used to "re-project" data from one geography into another #####
  overlap_function <- function( base_file, data_file, attribute_in_data_file, summation) {
    
    # Checking if the data file has no data, if so, just returning zeros
    if (nrow(data_file) == 0) return(0)
    
    # Checking if the attribute is in the file
    if (!(attribute_in_data_file %in% names(data_file))) stop(paste0('The selected attribute "', attribute_in_data_file, '" is not present in the data file'))
    # Both files must be in the same projection
    data_file <- st_transform(data_file, st_crs(base_file))
    
    # Giving each element in both the base and the data a unique ID
    base_file$base_id <- 1:nrow(base_file)
    data_file$data_id <- 1:nrow(data_file)
    
    # Giving each element in both the base and the data an area
    base_file$base_original_area <- unclass(st_area(base_file))
    data_file$data_original_area <- unclass(st_area(data_file))
    
    # Cleaning up the files (and saving space) by deleting the unnecessary columns
    base_file <- base_file %>% dplyr::select(base_id, base_original_area)
    data_file <- data_file %>% dplyr::select(data_id, data_original_area, {{attribute_in_data_file}})
    
    # Doing the actual intersection (this is where the magic happens)
    intersection <- base_file %>% st_intersection(data_file)
    
    # Getting the new area for each of the intersected polygons. This will be compared to the original area of the data file to figure out what proportion of that geography's value to sum
    intersection$new_area <- unclass(st_area(intersection))
    
    # Removing the geometry from the intersection file as we don't need it anymore and it slows everything down considerably if not removed
    intersection <- st_set_geometry(intersection, NULL)
    
    if(summation == "sum"){
      
      intersection <- intersection %>%
        mutate(area_proportion = new_area / data_original_area, # Obtaining the proportion of the original data geometry to sum into the new geographies
               new_attribute = .data[[attribute_in_data_file]] * area_proportion) %>% # Obtaining the correct proportion of that data value
        group_by(base_id) %>%
        summarise(new_attribute_sum = sum(new_attribute)) # Summarizing the new data value by base geography
      
    } else if (summation == "mean"){
      
      intersection <- intersection %>%
        mutate(new_attribute = .data[[attribute_in_data_file]] * new_area) %>% # Obtaining the correct proportion of that data value
        group_by(base_id) %>%
        summarise(new_attribute_sum = sum(new_attribute) / sum(new_area)) # Summarizing the new data value by base geography
      
    } else {
      
      stop("You didn't select 'mean' or 'sum' as your summation type.")
      
    }
    
    # Removing the geometry column, getting the data in the correct order
    final_values <- base_file %>%
      st_set_geometry(NULL) %>%
      left_join(intersection, by = 'base_id') %>%
      arrange(base_id)
    
    # Anywhere that errored, assign a value of zero
    final_values$new_attribute_sum[is.na(final_values$new_attribute_sum)] <- 0
    
    # Output the final values (just a matrix of values, not in a data frame or spatial data frame)
    return(final_values$new_attribute_sum)
    
  }
  
  
  ##### Getting ACS Data #####
  if (missing(county_value)){
    acs_data <- get_acs(
      geography = geography_value,
      variables = variables_value,
      state = state_value,
      geometry = TRUE,
      year = year_value,
      survey = survey_value,
      key = api_key_value
    )
  } else {
    acs_data <- get_acs(
      geography = geography_value,
      variables = variables_value,
      state = state_value,
      county = county_value,
      geometry = TRUE,
      year = year_value,
      survey = survey_value,
      key = api_key_value
    )
  }
  
  acs_data <- acs_data %>%
    rename(
      "{variables_value}" := estimate
    ) %>%
    select(
      -c(
        NAME,
        variable,
        moe
      )
    ) %>%
    st_transform(
      crs = 9822
    )
  
  
  ##### Making grid #####
  grid <- st_make_grid(
    x = acs_data,
    cellsize = 1609.34 * cell_size,
    square = FALSE
  ) %>%
    st_sf() %>%
    st_filter(
      y = acs_data,
      join = st_intersects
    )
  
  
  ##### Getting variables into grid #####
  grid$new_value <- suppressWarnings( overlap_function(grid, acs_data, variables_value, summation_type) )
  grid <- grid %>%
    rename(
      "{variables_value}" := new_value
    )
  
  return(grid)
  
  
}

##### Examples #####
library(tmap)

potential_variables_bg <- load_variables(2022, "acs5") %>% filter(geography == "block group")

#Median HH Income around Atlanta
med_hh_income_grid <- make_hexagons_acs(
  geography_value = "block group",
  api_key_value = "API KEY",
  variables_value = "B19013_001",
  summation_type = "mean",
  state_value = "Georgia",
  county_value = c("Fulton", "Dekalb", "Cobb", "Gwinnett", "Clayton"),
  year_value = 2022,
  survey_value = "acs5",
  cell_size = 0.5
)

tmap_mode("view")
tm_shape(med_hh_income_grid)+tm_fill(col = "B19013_001", alpha = 0.5)

#Total Population in a Few States
population_grid <- make_hexagons_acs(
  geography_value = "tract",
  api_key_value = "API KEY",
  variables_value = "B01001_001",
  summation_type = "sum",
  state_value = c("PA", "OH", "NY"),
  year_value = 2022,
  survey_value = "acs5",
  cell_size = 5
)

tmap_mode("view")
tm_shape(population_grid)+tm_fill(col = "B01001_001", style = "quantile", n = 10, alpha = 0.5)
