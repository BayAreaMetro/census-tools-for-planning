#Single Parent Poverty
#B17022

#Note this table only captures up to 185% FPL
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

ACS_variables <- c(Married_Under130FPL = "B17022_004",
                   SingleDad_Under130FPL = "B17022_011",
                   SingleMom_Under130FPL = "B17022_017",
                   Married_130to149FPL = "B17022_024",
                   SingleDad_130to149FPL = "B17022_031",
                   SingleMom_130to149FPL = "B17022_037",
                   Married_150to184FPL = "B17022_044",
                   SingleDad_150to184FPL = "B17022_051",
                   SingleMom_150to184FPL = "B17022_057",
                   Married_Over185FPL = "B17022_064",
                   SingleDad_Over185FPL = "B17022_071",
                   SingleMom_Over185FPL = "B17022_077")

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

SingleParent_County<- ACS_county_raw %>% mutate(
    Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
    Year=ACS_year,
    Married_Under185FPL = Married_Under130FPLE+Married_130to149FPLE+Married_150to184FPLE,
    Married_Under185FPL_MOE = round(sqrt(Married_Under130FPLM^2+Married_130to149FPLM^2+Married_150to184FPLM^2)),
    SingleDad_Under185FPL = SingleDad_Under130FPLE+SingleDad_130to149FPLE+SingleDad_150to184FPLE,
    SingleDad_Under185FPL_MOE = round(sqrt(SingleDad_Under130FPLM^2+SingleDad_130to149FPLM^2+SingleDad_150to184FPLM^2)),
    SingleMom_Under185FPL = SingleMom_Under130FPLE+SingleMom_130to149FPLE+SingleMom_150to184FPLE,
    SingleMom_Under185FPL_MOE = round(sqrt(SingleMom_Under130FPLM^2+SingleMom_130to149FPLM^2+SingleMom_150to184FPLM^2)),
    Married_Over185FPL = Married_Over185FPLE,
    Married_Over185FPL_MOE = Married_Over185FPLM,
    SingleDad_Over185FPL = SingleDad_Over185FPLE,
    SingleDad_Over185FPL_MOE = SingleDad_Over185FPLM,
    SingleMom_Over185FPL = SingleMom_Over185FPLE,
    SingleMom_Over185FPL_MOE = SingleMom_Over185FPLM)

#test if cities can provide meaningful results
SingleParent_City<- ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Married_Under185FPL = Married_Under130FPLE+Married_130to149FPLE+Married_150to184FPLE,
  Married_Under185FPL_MOE = round(sqrt(Married_Under130FPLM^2+Married_130to149FPLM^2+Married_150to184FPLM^2)),
  SingleDad_Under185FPL = SingleDad_Under130FPLE+SingleDad_130to149FPLE+SingleDad_150to184FPLE,
  SingleDad_Under185FPL_MOE = round(sqrt(SingleDad_Under130FPLM^2+SingleDad_130to149FPLM^2+SingleDad_150to184FPLM^2)),
  SingleMom_Under185FPL = SingleMom_Under130FPLE+SingleMom_130to149FPLE+SingleMom_150to184FPLE,
  SingleMom_Under185FPL_MOE = round(sqrt(SingleMom_Under130FPLM^2+SingleMom_130to149FPLM^2+SingleMom_150to184FPLM^2)),
  Married_Over185FPL = Married_Over185FPLE,
  Married_Over185FPL_MOE = Married_Over185FPLM,
  SingleDad_Over185FPL = SingleDad_Over185FPLE,
  SingleDad_Over185FPL_MOE = SingleDad_Over185FPLM,
  SingleMom_Over185FPL = SingleMom_Over185FPLE,
  SingleMom_Over185FPL_MOE = SingleMom_Over185FPLM) 
# too many zeros/MOEs too great as a percentage of the estimate to include
  
#for printing  
SingleParent_print<- SingleParent_County %>% select(Geo, Year, 
         Married_Under185FPL, 
         SingleDad_Under185FPL, 
         SingleMom_Under185FPL, 
         Married_Over185FPL,
         SingleDad_Over185FPL, 
         SingleMom_Over185FPL, 
         )
# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(SingleParent_print, file = "Single_parent/singleparent_poverty2018.csv")
