suppressMessages(library(dplyr))
library(tidycensus)

censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
# Import ACS library for variable inspection

ACS_table <- load_variables(year=2018, dataset="acs5", cache=TRUE) # change from acs1 to acs5 for small geographies

# Set up census variables for API

ACS_year <- 2018
ACS_product <- "acs5" # change from acs1 to acs 5 for income for displacement risk
baycounties <- c("01","13","41","55","75","81","85","95","97")
metros <- c("37980","47900","26420","33100","31080","16980","19100","35620","12060") # if 2014 or after metro data
statenumber="06"
