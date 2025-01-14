#Modeshare_CoC_groups

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

#Tables of Interest
#B08105 means of transport by race
#B08122 means of transport and poverty
#B08101 means of transport by age (65+)
#B08141 means of transport and # veh available
#B08113 means of transport and LEP

ACS_variables <- c(Black_DriveAlone= "B08105B_002",
                   Black_Carpool= "B08105B_003",
                   Black_PublicTransit= "B08105B_004",
                   Black_Walk= "B08105B_005",
                   Black_TaxiBikeOther= "B08105B_006",
                   Black_WorkfromHome= "B08105B_007",
                   Asian_DriveAlone= "B08105D_002",
                   Asian_Carpool= "B08105D_003",
                   Asian_PublicTransit= "B08105D_004",
                   Asian_Walk= "B08105D_005",
                   Asian_TaxiBikeOther= "B08105D_006",
                   Asian_WorkfromHome= "B08105D_007",
                   HPI_DriveAlone= "B08105E_002",
                   HPI_Carpool= "B08105E_003",
                   HPI_PublicTransit= "B08105E_004",
                   HPI_Walk= "B08105E_005",
                   HPI_TaxiBikeOther= "B08105E_006",
                   HPI_WorkfromHome= "B08105E_007",
                   WhiteNonHispanic_DriveAlone= "B08105H_002",
                   WhiteNonHispanic_Carpool= "B08105H_003",
                   WhiteNonHispanic_PublicTransit= "B08105H_004",
                   WhiteNonHispanic_Walk= "B08105H_005",
                   WhiteNonHispanic_TaxiBikeOther= "B08105H_006",
                   WhiteNonHispanic_WorkfromHome= "B08105H_007",
                   Latino_DriveAlone= "B08105I_002",
                   Latino_Carpool= "B08105I_003",
                   Latino_PublicTransit= "B08105I_004",
                   Latino_Walk= "B08105I_005",
                   Latino_TaxiBikeOther= "B08105I_006",
                   Latino_WorkfromHome= "B08105I_007",
                   Below100pFPL_DriveAlone= "B08122_006",
                   t100to149pFPL_DriveAlone= "B08122_007",
                   Over150FPL_DriveAlone= "B08122_008",
                   Below100pFPL_Carpool= "B08122_010",
                   t100to149pFPL_Carpool= "B08122_011",
                   Over150FPL_Carpool= "B08122_012",
                   Below100pFPL_PublicTransit= "B08122_014",
                   t100to149pFPL_PublicTransit= "B08122_015",
                   Over150FPL_PublicTransit= "B08122_016",
                   Below100pFPL_Walk= "B08122_018",
                   t100to149pFPL_Walk= "B08122_019",
                   Over150FPL_Walk= "B08122_020",
                   Below100pFPL_TaxiBikeOther= "B08122_022",
                   t100to149pFPL_TaxiBikeOther= "B08122_023",
                   Over150FPL_TaxiBikeOther= "B08122_024",
                   Below100pFPL_WorkfromHome= "B08122_026",
                   t100to149pFPL_WorkfromHome= "B08122_027",
                   Over150FPL_WorkfromHome= "B08122_028",
                   Senior_DriveAlone= "B08101_016",
                   Senior_Carpool= "B08101_024",
                   Senior_PublicTransit= "B08101_032",
                   Senior_Walk= "B08101_040",
                   Senior_TaxiBikeOther= "B08101_048",
                   Senior_WorkfromHome= "B08101_056",
                   ZV_DriveAlone= "B08141_007",
                   ZV_Carpool= "B08141_012",
                   ZV_PublicTransit= "B08141_017",
                   ZV_Walk= "B08141_022",
                   ZV_TaxiBikeOther= "B08141_027",
                   ZV_WorkfromHome= "B08141_032",
                   LEPSpanish_DriveAlone= "B08113_013",
                   LEPSpanish_Carpool= "B08113_021",
                   LEPSpanish_PublicTransit= "B08113_029",
                   LEPSpanish_Walk= "B08113_037",
                   LEPSpanish_TaxiBikeOther= "B08113_045",
                   LEPSpanish_WorkfromHome= "B08113_053",
                   LEPOther_DriveAlone= "B08113_016",
                   LEPOther_Carpool= "B08113_024",
                   LEPOther_PublicTransit= "B08113_032",
                   LEPOther_Walk= "B08113_040",
                   LEPOther_TaxiBikeOther= "B08113_048",
                   LEPOther_WorkfromHome= "B08113_056"
                   )

ACS_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product )

ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = ACS_variables,
                        year=ACS_year,
                        output="tidy",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 

#County
Modeshare_county <- ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Subgroup= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  Mode= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
  ) %>% rename(Population = estimate) %>% mutate(
  Subgroup = replace(Subgroup, Subgroup == 'LEPOther', 'LEP'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'LEPSpanish', 'LEP'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'ZV', 'Zero Vehicle'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'Below100pFPL', 'Below 100% FPL'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 't100to149pFPL', '100-150% FPL'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'Over150FPL', 'Over 150% FPL')
  )%>% select(Geo,Year,Subgroup,Mode,Population)

#City
Modeshare_city <- ACS_city_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Subgroup= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  Mode= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
) %>% rename(Population = estimate) %>% mutate(
  Subgroup = replace(Subgroup, Subgroup == 'LEPOther', 'LEP'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'LEPSpanish', 'LEP'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'ZV', 'Zero Vehicle'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'Below100pFPL', 'Below 100% FPL'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 't100to149pFPL', '100-150% FPL'))%>% mutate (
  Subgroup = replace(Subgroup, Subgroup == 'Over150FPL', 'Over 150% FPL')
  )%>% select(Geo,Year,Subgroup,Mode,Population)


# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(Modeshare_county, file = "Overall//Modeshare/CoCPop_county_modeshare_2018.csv")
write.csv(Modeshare_city, file = "Overall/Modeshare/CoCPop_city_modeshare_2018.csv")
