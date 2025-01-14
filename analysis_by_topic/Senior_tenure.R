#Senior by Tenure
#Followed by senior by tenure by age of building

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

ACS_variables <- c(Owner_15to24="B25007_003",
                   Owner_25to34="B25007_004",
                   Owner_35to44="B25007_005",
                   Owner_45to54="B25007_006",
                   Owner_55to59="B25007_007",
                   Owner_60to64="B25007_008",
                   Owner_65to74="B25007_009",
                   Owner_75to84="B25007_010",
                   Owner_Over85="B25007_011",
                   Renter_15to24="B25007_013",
                   Renter_25to34="B25007_014",
                   Renter_35to44="B25007_015",
                   Renter_45to54="B25007_016",
                   Renter_55to59="B25007_017",
                   Renter_60to64="B25007_018",
                   Renter_65to74="B25007_019",
                   Renter_75to84="B25007_020",
                   Renter_Over85="B25007_021")


ACS_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )

ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = ACS_variables,
                        year=ACS_year,
                        output="wide",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area


Age_tenure_county <- ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Owner_15to34= Owner_15to24E+Owner_25to34E,
  Owner_35to54= Owner_35to44E+Owner_45to54E,
  Owner_55to65= Owner_55to59E+Owner_60to64E,
  Owner_65to74= Owner_65to74E,
  Owner_75to84= Owner_75to84E,
  Owner_Over85= Owner_Over85E,
  Renter_15to34= Renter_15to24E+Renter_25to34E,
  Renter_35to54= Renter_35to44E+Renter_45to54E,
  Renter_55to65= Renter_55to59E+Renter_60to64E,
  Renter_65to74= Renter_65to74E,
  Renter_75to84= Renter_75to84E,
  Renter_Over85= Renter_Over85E)

Age_tenure_city <- ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Owner_15to34= Owner_15to24E+Owner_25to34E,
  Owner_35to54= Owner_35to44E+Owner_45to54E,
  Owner_55to65= Owner_55to59E+Owner_60to64E,
  Owner_65to74= Owner_65to74E,
  Owner_75to84= Owner_75to84E,
  Owner_Over85= Owner_Over85E,
  Renter_15to34= Renter_15to24E+Renter_25to34E,
  Renter_35to54= Renter_35to44E+Renter_45to54E,
  Renter_55to65= Renter_55to59E+Renter_60to64E,
  Renter_65to74= Renter_65to74E,
  Renter_75to84= Renter_75to84E,
  Renter_Over85= Renter_Over85E)

#Prep for printing

age_tenure_county_print<- Age_tenure_county %>% select(Geo, Year, 
                                                       Owner_15to34,
                                                       Owner_35to54,
                                                       Owner_55to65,
                                                       Owner_65to74,
                                                       Owner_75to84,
                                                       Owner_Over85,
                                                       Renter_15to34,
                                                       Renter_35to54,
                                                       Renter_55to65,
                                                       Renter_65to74,
                                                       Renter_75to84,
                                                       Renter_Over85)

age_tenure_city_print<- Age_tenure_city %>% select(Geo, Year, 
                                                       Owner_15to34,
                                                       Owner_35to54,
                                                       Owner_55to65,
                                                       Owner_65to74,
                                                       Owner_75to84,
                                                       Owner_Over85,
                                                       Renter_15to34,
                                                       Renter_35to54,
                                                       Renter_55to65,
                                                       Renter_65to74,
                                                       Renter_75to84,
                                                       Renter_Over85)
# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(age_tenure_county_print, file = "Senior/age_tenure_county2018.csv")
write.csv(age_tenure_city_print, file = "Senior/age_tenure_city2018.csv")

#Collect age/tenure/building age variables
Age_Ten_BlgAge_ACS_variables <- c(Owner_15to34_2014later="B25126_004",
                                  Owner_15to34_2010to2013="B25126_005",
                                  Owner_15to34_2000to2009="B25126_006",
                                  Owner_15to34_1990to1999="B25126_007",
                                  Owner_15to34_1980to1989="B25126_008",
                                  Owner_15to34_1970to1979="B25126_009",
                                  Owner_15to34_1960to1969="B25126_010",
                                  Owner_15to34_1950to1959="B25126_011",
                                  Owner_15to34_1940to1949="B25126_012",
                                  Owner_15to34_before1939="B25126_013",
                                  Owner_35to64_2014later="B25126_015",
                                  Owner_35to64_2010to2013="B25126_016",
                                  Owner_35to64_2000to2009="B25126_017",
                                  Owner_35to64_1990to1999="B25126_018",
                                  Owner_35to64_1980to1989="B25126_019",
                                  Owner_35to64_1970to1979="B25126_020",
                                  Owner_35to64_1960to1969="B25126_021",
                                  Owner_35to64_1950to1959="B25126_022",
                                  Owner_35to64_1940to1949="B25126_023",
                                  Owner_35to64_before1939="B25126_024",
                                  Owner_Over65_2014later="B25126_026",
                                  Owner_Over65_2010to2013="B25126_027",
                                  Owner_Over65_2000to2009="B25126_028",
                                  Owner_Over65_1990to1999="B25126_029",
                                  Owner_Over65_1980to1989="B25126_030",
                                  Owner_Over65_1970to1979="B25126_031",
                                  Owner_Over65_1960to1969="B25126_032",
                                  Owner_Over65_1950to1959="B25126_033",
                                  Owner_Over65_1940to1949="B25126_034",
                                  Owner_Over65_before1939="B25126_035",
                                  Renter_15to34_2014later="B25126_038",
                                  Renter_15to34_2010to2013="B25126_039",
                                  Renter_15to34_2000to2009="B25126_040",
                                  Renter_15to34_1990to1999="B25126_041",
                                  Renter_15to34_1980to1989="B25126_042",
                                  Renter_15to34_1970to1979="B25126_043",
                                  Renter_15to34_1960to1969="B25126_044",
                                  Renter_15to34_1950to1959="B25126_045",
                                  Renter_15to34_1940to1949="B25126_046",
                                  Renter_15to34_before1939="B25126_047",
                                  Renter_35to64_2014later="B25126_049",
                                  Renter_35to64_2010to2013="B25126_050",
                                  Renter_35to64_2000to2009="B25126_051",
                                  Renter_35to64_1990to1999="B25126_052",
                                  Renter_35to64_1980to1989="B25126_053",
                                  Renter_35to64_1970to1979="B25126_054",
                                  Renter_35to64_1960to1969="B25126_055",
                                  Renter_35to64_1950to1959="B25126_056",
                                  Renter_35to64_1940to1949="B25126_057",
                                  Renter_35to64_before1939="B25126_058",
                                  Renter_Over65_2014later="B25126_060",
                                  Renter_Over65_2010to2013="B25126_061",
                                  Renter_Over65_2000to2009="B25126_062",
                                  Renter_Over65_1990to1999="B25126_063",
                                  Renter_Over65_1980to1989="B25126_064",
                                  Renter_Over65_1970to1979="B25126_065",
                                  Renter_Over65_1960to1969="B25126_066",
                                  Renter_Over65_1950to1959="B25126_067",
                                  Renter_Over65_1940to1949="B25126_068",
                                  Renter_Over65_before1939="B25126_069")

ACS_blgage_county_raw <- get_acs(geography = "county", variables = Age_Ten_BlgAge_ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )

ACS_blgage_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = Age_Ten_BlgAge_ACS_variables,
                        year=ACS_year,
                        output="wide",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area


Age_tenure_blgage_county <- ACS_blgage_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Owner_15to34_2000later= Owner_15to34_2014laterE+Owner_15to34_2010to2013E+Owner_15to34_2000to2009E,
  Owner_15to34_1980to1999= Owner_15to34_1990to1999E+Owner_15to34_1980to1989E,
  Owner_15to34_1960to1979= Owner_15to34_1970to1979E+Owner_15to34_1960to1969E,
  Owner_15to34_1940to1959= Owner_15to34_1950to1959E+Owner_15to34_1940to1949E,
  Owner_15to34_before1939= Owner_15to34_before1939E,
  Owner_35to64_2000later= Owner_35to64_2014laterE+Owner_35to64_2010to2013E+Owner_35to64_2000to2009E,
  Owner_35to64_1980to1999= Owner_35to64_1990to1999E+Owner_35to64_1980to1989E,
  Owner_35to64_1960to1979= Owner_35to64_1970to1979E+Owner_35to64_1960to1969E,
  Owner_35to64_1940to1959= Owner_35to64_1950to1959E+Owner_35to64_1940to1949E,
  Owner_35to64_before1939= Owner_35to64_before1939E,
  Owner_Over65_2000later= Owner_Over65_2014laterE+Owner_Over65_2010to2013E+Owner_Over65_2000to2009E,
  Owner_Over65_1980to1999= Owner_Over65_1990to1999E+Owner_Over65_1980to1989E,
  Owner_Over65_1960to1979= Owner_Over65_1970to1979E+Owner_Over65_1960to1969E,
  Owner_Over65_1940to1959= Owner_Over65_1950to1959E+Owner_Over65_1940to1949E,
  Owner_Over65_before1939= Owner_Over65_before1939E,
  Renter_15to34_2000later= Renter_15to34_2014laterE+Renter_15to34_2010to2013E+Renter_15to34_2000to2009E,
  Renter_15to34_1980to1999= Renter_15to34_1990to1999E+Renter_15to34_1980to1989E,
  Renter_15to34_1960to1979= Renter_15to34_1970to1979E+Renter_15to34_1960to1969E,
  Renter_15to34_1940to1959= Renter_15to34_1950to1959E+Renter_15to34_1940to1949E,
  Renter_15to34_before1939= Renter_15to34_before1939E,
  Renter_35to64_2000later= Renter_35to64_2014laterE+Renter_35to64_2010to2013E+Renter_35to64_2000to2009E,
  Renter_35to64_1980to1999= Renter_35to64_1990to1999E+Renter_35to64_1980to1989E,
  Renter_35to64_1960to1979= Renter_35to64_1970to1979E+Renter_35to64_1960to1969E,
  Renter_35to64_1940to1959= Renter_35to64_1950to1959E+Renter_35to64_1940to1949E,
  Renter_35to64_before1939= Renter_35to64_before1939E,
  Renter_Over65_2000later= Renter_Over65_2014laterE+Renter_Over65_2010to2013E+Renter_Over65_2000to2009E,
  Renter_Over65_1980to1999= Renter_Over65_1990to1999E+Renter_Over65_1980to1989E,
  Renter_Over65_1960to1979= Renter_Over65_1970to1979E+Renter_Over65_1960to1969E,
  Renter_Over65_1940to1959= Renter_Over65_1950to1959E+Renter_Over65_1940to1949E,
  Renter_Over65_before1939= Renter_Over65_before1939E)

#Assume that MOEs will be too large for cities
#Prep for printing

age_tenure_blgage_county_print<- Age_tenure_blgage_county %>% select(Geo, Year, 
                                                                     Owner_15to34_2000later,
                                                                     Owner_15to34_1980to1999,
                                                                     Owner_15to34_1960to1979,
                                                                     Owner_15to34_1940to1959,
                                                                     Owner_15to34_before1939,
                                                                     Owner_35to64_2000later,
                                                                     Owner_35to64_1980to1999,
                                                                     Owner_35to64_1960to1979,
                                                                     Owner_35to64_1940to1959,
                                                                     Owner_35to64_before1939,
                                                                     Owner_Over65_2000later,
                                                                     Owner_Over65_1980to1999,
                                                                     Owner_Over65_1960to1979,
                                                                     Owner_Over65_1940to1959,
                                                                     Owner_Over65_before1939,
                                                                     Renter_15to34_2000later,
                                                                     Renter_15to34_1980to1999,
                                                                     Renter_15to34_1960to1979,
                                                                     Renter_15to34_1940to1959,
                                                                     Renter_15to34_before1939,
                                                                     Renter_35to64_2000later,
                                                                     Renter_35to64_1980to1999,
                                                                     Renter_35to64_1960to1979,
                                                                     Renter_35to64_1940to1959,
                                                                     Renter_35to64_before1939,
                                                                     Renter_Over65_2000later,
                                                                     Renter_Over65_1980to1999,
                                                                     Renter_Over65_1960to1979,
                                                                     Renter_Over65_1940to1959,
                                                                     Renter_Over65_before1939)

# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(age_tenure_blgage_county_print, file = "Senior/age_tenure_blgage_county2018.csv")
