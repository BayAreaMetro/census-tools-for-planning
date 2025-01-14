#Percent change over time in minorities

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
v9o <- load_variables(1990, "sf1", cache = TRUE)
view(voo)

##FOR ANUP 
ACS_variables2 <- c(Total= "B03002_001",
                    White= "B03002_003",
                    Black= "B03002_004",
                    AmIndAlNat= "B03002_005",
                    Asian= "B03002_006",
                    HPI= "B03002_007",
                    OtherRace= "B03002_008",
                    Twoplusraces= "B03002_009",
                    Latino= "B03002_012")

ACS18_coccheck <- get_acs(geography = "county", variables = ACS_variables2,
                             state = statenumber, county=baycounties,
                             year=ACS_year,
                             output="wide",
                             survey = ACS_product )

# Write out CSVs 
setwd("C:/Users/jhalpern/Box/Horizon and Plan Bay Area 2050/Equity and Performance/7_Analysis/Demographics/Outputs")
write.csv(ACS18_coccheck, file = "Minorities/racebycity_longitudinal.csv")

#BACK TO REGULAR CODEBOOK
#collect race/ethnicity points for ACS plus 2000 and 1990 census
ACS_variables <- c(White= "B03002_003",
                   Black= "B03002_004",
                   AmIndAlNat= "B03002_005",
                   Asian= "B03002_006",
                   HPI= "B03002_007",
                   OtherRace= "B03002_008",
                   Twoplusraces= "B03002_009",
                   Latino= "B03002_012")

Cen2000_variables<- c(White= "P008003",
                      Black= "P008004",
                      AmIndAlNat= "P008005",
                      Asian= "P008006",
                      HPI= "P008007",
                      OtherRace= "P008008",
                      Twoplusraces= "P008009",
                      Latino= "P008010")

Cen1990_variables<- c(White= "P0100001",
                      Black= "P0100002",
                      AmIndAlNat= "P0100003",
                      API= "P0100004",
                      OtherRace= "P0100005",
                      Latino= "P0080001")

#call each of the respective years
#ACS2014-2018
ACS18_county_raw2 <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product )

ACS18_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = ACS_variables,
                        year=ACS_year,
                        output="tidy",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area

#For CoC Update memo ACS2010-2014
ACS14_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                            state = statenumber, county=baycounties,
                            year=2014,
                            output="tidy",
                            survey = ACS_product )

#ACS2009-2013
ACS13_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                            state = statenumber, county=baycounties,
                            year=2013,
                            output="tidy",
                            survey = ACS_product )

ACS13_city_raw <- get_acs(state = statenumber,
                          geography = "place", variables = ACS_variables,
                          year=2013,
                          output="tidy",
                          survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area

#Census2000
Cen2000_county_raw <- get_decennial(geography = "county", variables = Cen2000_variables,
                            state = statenumber, county=baycounties,
                            year=2000,
                            output="tidy")

Cen2000_city_raw <- get_decennial(state = statenumber,
                          geography = "place", variables = Cen2000_variables,
                          year=2000,
                          output="tidy" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area

#Census1990
Cen1990_county_raw <- get_decennial(geography = "county", variables = Cen1990_variables,
                                    state = statenumber, county=baycounties,
                                    year=1990,
                                    output="tidy")

Cen1990_city_raw <- get_decennial(state = statenumber,
                                  geography = "place", variables = Cen1990_variables,
                                  year=1990,
                                  output="tidy" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area

#Transform the tbls so all have 1) the same columns 2) the same nomenclature 3) add a year and product columns 
#Asian and PI are merged because the 1990 census does not separate the two
#^deleted separation
ACS18_county_prep <- ACS18_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2018,
  Product='ACS 5yr'
  ) %>% rename(Population = estimate) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'Twoplusraces', 'Other'))%>% mutate (
  )%>% select(GEOID,Geo,Year,Race,Population,Product)

ACS14_county_prep <- ACS14_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2014,
  Product='ACS 5yr'
) %>% rename(Population = estimate) %>% rename(Race = variable) %>% mutate(
  Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'Twoplusraces', 'Other'))%>% mutate (
      )%>% select(GEOID,Geo,Year,Race,Population,Product)


ACS13_county_prep <- ACS13_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2013,
  Product='ACS 5yr'
  ) %>% rename(Population = estimate) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'Twoplusraces', 'Other'))%>% mutate (
        
  )%>% select(GEOID,Geo,Year,Race,Population,Product)

Cen2000_county_prep <- Cen2000_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2000,
  Product='Census SF1'
) %>% rename(Population = value) %>% rename(Race = variable) %>% mutate(
  Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'Twoplusraces', 'Other'))%>% mutate (
      
    )%>% select(GEOID,Geo,Year,Race,Population,Product)

Cen1990_county_prep <- Cen1990_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=1990,
  Product='Census SF1'
) %>% rename(Population = value) %>% rename(Race = variable) %>% mutate(
  Race = replace(Race, Race == 'API', 'Asian'))%>% mutate (
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
  Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
  )%>% select(GEOID,Geo,Year,Race,Population,Product)

#Combine calls into one df
Race_County <- rbind(ACS18_county_prep,ACS13_county_prep,Cen2000_county_prep,Cen1990_county_prep)
#,Cen2000_county_prep,Cen1990_county_prep
Race_CountyUpdate <- rbind(ACS18_county_prep,ACS14_county_prep)

#now repeat for cities
ACS18_city_prep <- ACS18_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2018,
  Product='ACS 5yr'
) %>% rename(Population = estimate) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    )%>% select(GEOID,Geo,Year,Race,Population,Product)

ACS13_city_prep <- ACS13_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2013,
  Product='ACS 5yr'
) %>% rename(Population = estimate) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
    Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    )%>% select(GEOID,Geo,Year,Race,Population,Product)

Cen2000_city_prep <- Cen2000_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=2000,
  Product='Census SF1'
) %>% rename(Population = value) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
      Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
        Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    )%>% select(GEOID,Geo,Year,Race,Population,Product)

Cen1990_city_prep <- Cen1990_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=1990,
  Product='Census SF1'
) %>% rename(Population = value) %>% rename(Race = variable) %>% mutate(
    Race = replace(Race, Race == 'HPI', 'Other'))%>% mutate (
      Race = replace(Race, Race == 'OtherRace', 'Other'))%>% mutate (
        Race = replace(Race, Race == 'AmIndAlNat', 'Other'))%>% mutate (
    )%>% select(GEOID,Geo,Year,Race,Population,Product)

#compare number of cities to ensure consistency
length(unique(ACS18_city_prep[["Geo"]])) #101
length(unique(ACS13_city_prep[["Geo"]])) #101
length(unique(Cen2000_city_prep[["Geo"]])) #100
length(unique(Cen1990_city_prep[["Geo"]])) #99
#2000 is missing 1 city while 1990 is missing 2. Find the missing cities
ACS18_city_prep[["Geo"]][!(ACS18_city_prep[["Geo"]] %in% ACS13_city_prep[["Geo"]])]
ACS18_city_prep[["Geo"]][!(ACS18_city_prep[["Geo"]] %in% Cen2000_city_prep[["Geo"]])]
ACS18_city_prep[["Geo"]][!(ACS18_city_prep[["Geo"]] %in% Cen1990_city_prep[["Geo"]])]
#test reverse
Cen2000_city_prep[["Geo"]][!(Cen2000_city_prep[["Geo"]] %in% ACS18_city_prep[["Geo"]])] #Yountville is town in 2000 and city in 2013/2018, Moraga town not present
Cen1990_city_prep[["Geo"]][!(Cen1990_city_prep[["Geo"]] %in% ACS18_city_prep[["Geo"]])]
#American Canyon CDP(1990)/city(2018),Danville city(1990)/town(2018), Oakley CDP(1990)/city(2018),Windsor CDP(1990)/town(2018), Campbell city and Moraga town not present 

#Edit Census 2000 and 1990 jurisdictions to match 
Cen2000_city_alignment <- Cen2000_city_prep %>% mutate(
    Geo = replace(Geo, Geo == 'Yountville town', 'Yountville city'))

Cen1990_city_alignment <- Cen1990_city_prep %>% mutate(
  Geo = replace(Geo, Geo == 'Yountville town', 'Yountville city'),
  Geo = replace(Geo, Geo == 'American Canyon CDP', 'American Canyon city'),
  Geo = replace(Geo, Geo == 'Danville city', 'Danville town'),
  Geo = replace(Geo, Geo == 'Oakley CDP', 'Oakley city'),
  Geo = replace(Geo, Geo == 'Windsor CDP', 'Windsor town')
  )
#test
ACS18_city_prep[["Geo"]][!(ACS18_city_prep[["Geo"]] %in% Cen2000_city_alignment[["Geo"]])] #Only Moraga town missing
ACS18_city_prep[["Geo"]][!(ACS18_city_prep[["Geo"]] %in% Cen1990_city_alignment[["Geo"]])] #Only Campbell City and Moraga town missing

#combine into one df
Race_City <- rbind(ACS18_city_prep,ACS13_city_prep,Cen2000_city_alignment,Cen1990_city_alignment)

# Write out CSVs 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(Race_City, file = "Minorities/racebycity_longitudinal.csv")
write.csv(Race_County, file = "Minorities/racebycounty_longitudinal_0820.csv")
write.csv(Race_CountyUpdate, file = "Minorities/racebycounty_longitudinal3.csv")
