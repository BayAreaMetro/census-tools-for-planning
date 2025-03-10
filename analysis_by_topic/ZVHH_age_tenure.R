#ZVHH by Age and Tenure

# Import Libraries
library(dplyr)
library(tidycensus)
library(httr)
library(jsonlite)
library(lubridate)

#set up Census API  
censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")


ACS_year=2018
#note: add additional years for changes
ACS_product="acs5"
baycities = c("0600562","0600674","0601640","0602252","0603092","0605108","0605164","0605290",
              "0606000","0608142","0608310","0609066","0609892","0610345","0613882","0614190",
              "0614736","0616000","0616462","0616560","0617610","0617918","0617988","0619402",
              "0620018","0620956","0621796","0622594","0623168","0623182","0625338","0626000",
              "0629504","0631708","0633000","0633056","0633308","0633798","0639122","0640438",
              "0641992","0643280","0643294","0644112","0646114","0646870","0647710","0647486",
              "0647766","0648956","0649187","0649278","0649670","0650258","0650916","0652582",
              "0653000","0653070","0654232","0654806","0655282","0656784","0656938","0657288",
              "0657456","0657764","0657792","0658380","0660102","0660620","0660984","0662546",
              "0662980","0664434","0665028","0665070","0667000","0668000","0668084","0668252",
              "0668294","0668364","0668378","0669084","0670098","0670280","0670364","0670770",
              "0672646","0673262","0664140","0675630","0677000","0678666","0681204","0681554",
              "0681666","0683346","0685922","0686440","0686930")
baycounties=c("01","13","41","55","75","81","85","95","97")
statenumber="06"
metros = c("37980","47900","26420","33100","31080","16980","19100","35620","12060") # if 2014 or after metro data

#If need to look at variables available
v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
#v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)

ACS_variables <- c(Owner_15to34_0Veh="B25045_004",
                   Owner_35to64_0Veh="B25045_005",
                   Owner_Over65_0Veh="B25045_006",
                   Owner_15to34_1PlusVeh="B25045_008",
                   Owner_35to64_1PlusVeh="B25045_009",
                   Owner_Over65_1PlusVeh="B25045_010",
                   Renter_15to34_0Veh="B25045_013",
                   Renter_35to64_0Veh="B25045_014",
                   Renter_Over65_0Veh="B25045_015",
                   Renter_15to34_1PlusVeh="B25045_017",
                   Renter_35to64_1PlusVeh="B25045_018",
                   Renter_Over65_1PlusVeh="B25045_019")

#county
ACS_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product )                  

Veh_tenure_age_county <- ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Tenure= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  Age= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
  Veh_Available= sapply((strsplit(as.character(variable),'_')),function(x) x[3])
  ) %>% rename(Population = estimate) %>% select(Geo,GEOID, Year,Age,Tenure,Veh_Available,Population)

#city
ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = ACS_variables,
                        year=ACS_year,
                        output="tidy",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area

Veh_tenure_age_city <- ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Tenure= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  Age= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
  Veh_Available= sapply((strsplit(as.character(variable),'_')),function(x) x[3])
) %>% rename(Population = estimate) %>% select(Geo,GEOID, Year,Age,Tenure,Veh_Available,Population)

# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(Veh_tenure_age_county, file = "ZVHH/ZVHH_age_tenure_county2018.csv")
write.csv(Veh_tenure_age_city, file = "ZVHH/ZVHH_age_tenure_city2018.csv")

