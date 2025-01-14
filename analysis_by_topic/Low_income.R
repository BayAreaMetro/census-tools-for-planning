#Low Income (overall + seniors age 75+)
install.packages("openxlsx")
# Import Libraries
library(dplyr)
library(tidycensus)
library(httr)
library(jsonlite)
library(lubridate)
library(openxlsx)

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

Total_ACS_variables <- c(Under50p = "C17002_002",
                   p50pto99p = "C17002_003",
                   p100pto124p = "C17002_004",
                   p125pto149p= "C17002_005",
                   p150pto184p = "C17002_006",
                   p185pto199p = "C17002_007",
                   Over200p= "C17002_008")

Senior_ACS_variables <- c(Under50p = "B17024_120",
                         p50pto74p = "B17024_121",
                         p75pto99p = "B17024_122",
                         p100pto124p = "B17024_123",
                         p125pto149p= "B17024_124",
                         p150pto174p = "B17024_125",
                         p175pto184p = "B17024_126",
                         p185pto199p = "B17024_127",
                         p200pto299p = "B17024_128",
                         p300pto399p = "B17024_129",
                         p400pto499p= "B17024_130",
                         Over500p= "B17024_131")

#City Raw (for Overall and Senior)
Total_ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = Total_ACS_variables,
                        year=ACS_year,
                        output="tidy",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 

Senior_ACS_city_raw <- get_acs(state = statenumber,
                              geography = "place", variables = Senior_ACS_variables,
                              year=ACS_year,
                              output="tidy",
                              survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 
#Senior by city has MOEs that are too high for useful analysis and C17024 (summarized version) is not available. Try at county level

Senior_ACS_county_raw <- get_acs(geography = "county", variables = Senior_ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product )

Total_ACS_county_raw <- get_acs(geography = "county", variables = Total_ACS_variables,
                                 state = statenumber, county=baycounties,
                                 year=ACS_year,
                                 output="tidy",
                                 survey = ACS_product )

Low_Income_total_city <- Total_ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Low_Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  ) %>% rename(Population = estimate) %>% mutate(
  Low_Income = replace(Low_Income, Low_Income == 'Under50p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p50pto99p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p100pto124p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p125pto149p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p150pto184p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p185pto199p', 'Under 200p FPL'))%>%select(Geo,GEOID,Year,Low_Income,Population)

Low_Income_senior_city <- Senior_ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Low_Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
) %>% rename(Population = estimate) %>% mutate(
  Low_Income = replace(Low_Income, Low_Income == 'Under50p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p50pto74p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p75pto99p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p100pto124p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p125pto149p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p150pto174p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p175pto184p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p185pto199p', 'Under 200p FPL'))%>% mutate ( 
  Low_Income = replace(Low_Income, Low_Income == 'p200pto299p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p300pto399p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p400pto499p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'Over500p', 'Over 200p FPL'))%>%select(Geo,GEOID,Year,Low_Income,Population)

Low_Income_senior_county <- Senior_ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Low_Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
) %>% rename(Population = estimate) %>% mutate(
  Low_Income = replace(Low_Income, Low_Income == 'Under50p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p50pto74p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p75pto99p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p100pto124p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p125pto149p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p150pto174p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p175pto184p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p185pto199p', 'Under 200p FPL'))%>% mutate ( 
  Low_Income = replace(Low_Income, Low_Income == 'p200pto299p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p300pto399p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p400pto499p', 'Over 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'Over500p', 'Over 200p FPL'))%>%select(Geo,GEOID,Year,Low_Income,Population)

Low_Income_total_county <- Total_ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Low_Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
) %>% rename(Population = estimate) %>% mutate(
  Low_Income = replace(Low_Income, Low_Income == 'Under50p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p50pto99p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p100pto124p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p125pto149p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p150pto184p', 'Under 200p FPL'))%>% mutate (
  Low_Income = replace(Low_Income, Low_Income == 'p185pto199p', 'Under 200p FPL'))%>%select(Geo,GEOID,Year,Low_Income,Population)

# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.xlsx(Low_Income_total_city, file = "Low_Income/lowincome_city_total_2018.xlsx", colNames = TRUE, sheetName = "low_income_totalpop")
write.xlsx(Low_Income_senior_city, file = "Senior/lowincome_city_senior_2018.xlsx", colNames = TRUE, sheetName = "low_income_seniors")
write.xlsx(Low_Income_senior_county, file = "Senior/lowincome_county_senior_2018.xlsx", colNames = TRUE, sheetName = "low_income_COUNTY_seniors")
write.xlsx(Low_Income_total_county, file = "Low_Income/lowincome_county_total_2018.xlsx", colNames = TRUE, sheetName = "low_income_COUNTY_totalpop")
