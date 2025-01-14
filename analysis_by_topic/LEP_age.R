#Age and LEP
#The universe is all pop over 5, an approximation of full population to perform percent LEP by tract
#Outstanding Q: though this is static, there is also a 1yr estimate that could be used to show change over time
#LEP is defined here as speaks a language other than English and less than "very well"
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

#If need to look at variables available
v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
#v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)

#pull county, tract, city data
ACS_tract_raw <- get_acs(state = statenumber,
                         county = baycounties,
                         geography = "tract",
                         table = "B16004",
                         year=ACS_year,
                         output="tidy",
                         survey = ACS_product ) 

ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", table = "B16004",
                        year=ACS_year,
                        output="tidy",
                        survey = ACS_product ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 

#County
ACS_county_raw <- get_acs(geography = "county",table = "B16004",
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )

LEP_age_county <-ACS_county_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  NonLEP_5to17 = B16004_003E+B16004_005E+B16004_010E+B16004_015E+B16004_020E,
  LEP_5to17 = B16004_006E+B16004_007E+B16004_008E+B16004_011E+B16004_012E+B16004_013E+B16004_016E+B16004_017E+B16004_018E+B16004_021E+B16004_022E+B16004_023E,
  NonLEP_18to64 = B16004_025E+B16004_027E+B16004_032E+B16004_037E+B16004_042E,
  LEP_18to64 = B16004_028E+B16004_029E+B16004_030E+B16004_033E+B16004_034E+B16004_035E+B16004_038E+B16004_039E+B16004_040E+B16004_043E+B16004_044E+B16004_045E,
  NonLEP_Over65 = B16004_047E+B16004_049E+B16004_054E+B16004_059E+B16004_064E,
  LEP_Over65 = B16004_050E+B16004_051E+B16004_052E+B16004_055E+B16004_056E+B16004_057E+B16004_060E+B16004_061E+B16004_062E+B16004_065E+B16004_066E+B16004_067E
)%>% select(Geo,Year,NonLEP_5to17,LEP_5to17,NonLEP_18to64,LEP_18to64,NonLEP_Over65,LEP_Over65)

# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(LEP_age_county, file = "LEP/lep_age2018wide.csv")
