#Limited English Proficiency

# Import Libraries
library(dplyr)
library(tidycensus)

censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

ACS_year=2018
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
#v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
#v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)

#County
ACS_county_raw <- get_acs(geography = "county",table = "B08113",
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )


LEP_mode_county <-ACS_county_raw %>% mutate (
          Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
          Year=ACS_year,
          DriveAlone_NonLEP = B08113_010E+B08113_012E+B08113_015E,
          DriveAlone_NonLEP_MOE = sqrt(B08113_010M^2+B08113_012M^2+B08113_015M^2),
          DriveAlone_LEP = B08113_013E+B08113_016E,
          DriveAlone_LEP_MOE  = sqrt(B08113_013M^2+B08113_016M^2),
          Carpool_NonLEP = B08113_018E+B08113_020E+B08113_023E,
          Carpool_NonLEP_MOE = sqrt(B08113_018M^2+B08113_020M^2+B08113_023M^2),
          Carpool_LEP = B08113_021E+B08113_024E,
          Carpool_LEP_MOE = sqrt(B08113_021M^2+B08113_024M^2),
          PublicTransport_NonLEP = B08113_026E+B08113_028E+B08113_031E,
          PublicTransport_NonLEP_MOE = sqrt(B08113_026M^2+B08113_028M^2+B08113_031M^2),
          PublicTransport_LEP = B08113_029E+B08113_032E,
          PublicTransport_LEP_MOE = sqrt(B08113_029M^2+B08113_032M^2),
          Walk_NonLEP = B08113_034E+B08113_036E+B08113_039E,
          Walk_NonLEP_MOE = sqrt(B08113_034M^2+B08113_036M^2+B08113_039M^2),
          Walk_LEP = B08113_037E+B08113_040E,
          Walk_LEP_MOE = sqrt(B08113_037M^2+B08113_040M^2),
          TaxiBikeOther_NonLEP = B08113_042E+B08113_044E+B08113_047E,
          TaxiBikeOther_NonLEP_MOE = sqrt(B08113_042M^2+B08113_044M^2+B08113_047M^2),
          TaxiBikeOther_LEP = B08113_045E+B08113_048E,
          TaxiBikeOther_LEP_MOE = sqrt(B08113_045M^2+B08113_048M^2),
          WorkatHome_NonLEP = B08113_050E+B08113_052E+B08113_055E,
          WorkatHome_NonLEP_MOE = sqrt(B08113_050M^2+B08113_052M^2+B08113_055M^2),
          WorkatHome_LEP = B08113_053E+B08113_056E,
          WorkatHome_LEP_MOE = sqrt(B08113_053M^2+B08113_056M^2)
                       ) %>%
          select(Geo,Year,
                 DriveAlone_NonLEP,DriveAlone_NonLEP_MOE,DriveAlone_LEP,DriveAlone_LEP_MOE,
                 Carpool_NonLEP,Carpool_NonLEP_MOE,Carpool_LEP,Carpool_LEP_MOE, 
                 PublicTransport_NonLEP,PublicTransport_NonLEP_MOE,PublicTransport_LEP,PublicTransport_LEP_MOE,
                 Walk_NonLEP, Walk_NonLEP_MOE,Walk_LEP,Walk_LEP_MOE, 
                 TaxiBikeOther_NonLEP,TaxiBikeOther_NonLEP_MOE,TaxiBikeOther_LEP,TaxiBikeOther_LEP_MOE,
                 WorkatHome_NonLEP,WorkatHome_NonLEP_MOE,WorkatHome_LEP,WorkatHome_LEP_MOE)

#City
ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", table = "B08113",
                        year=ACS_year,
                        output="wide",
                        survey = "acs5" ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 

LEP_mode_city <-ACS_city_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  DriveAlone_NonLEP = B08113_010E+B08113_012E+B08113_015E,
  DriveAlone_NonLEP_MOE = sqrt(B08113_010M^2+B08113_012M^2+B08113_015M^2),
  DriveAlone_LEP = B08113_013E+B08113_016E,
  DriveAlone_LEP_MOE  = sqrt(B08113_013M^2+B08113_016M^2),
  Carpool_NonLEP = B08113_018E+B08113_020E+B08113_023E,
  Carpool_NonLEP_MOE = sqrt(B08113_018M^2+B08113_020M^2+B08113_023M^2),
  Carpool_LEP = B08113_021E+B08113_024E,
  Carpool_LEP_MOE = sqrt(B08113_021M^2+B08113_024M^2),
  PublicTransport_NonLEP = B08113_026E+B08113_028E+B08113_031E,
  PublicTransport_NonLEP_MOE = sqrt(B08113_026M^2+B08113_028M^2+B08113_031M^2),
  PublicTransport_LEP = B08113_029E+B08113_032E,
  PublicTransport_LEP_MOE = sqrt(B08113_029M^2+B08113_032M^2),
  Walk_NonLEP = B08113_034E+B08113_036E+B08113_039E,
  Walk_NonLEP_MOE = sqrt(B08113_034M^2+B08113_036M^2+B08113_039M^2),
  Walk_LEP = B08113_037E+B08113_040E,
  Walk_LEP_MOE = sqrt(B08113_037M^2+B08113_040M^2),
  TaxiBikeOther_NonLEP = B08113_042E+B08113_044E+B08113_047E,
  TaxiBikeOther_NonLEP_MOE = sqrt(B08113_042M^2+B08113_044M^2+B08113_047M^2),
  TaxiBikeOther_LEP = B08113_045E+B08113_048E,
  TaxiBikeOther_LEP_MOE = sqrt(B08113_045M^2+B08113_048M^2),
  WorkatHome_NonLEP = B08113_050E+B08113_052E+B08113_055E,
  WorkatHome_NonLEP_MOE = sqrt(B08113_050M^2+B08113_052M^2+B08113_055M^2),
  WorkatHome_LEP = B08113_053E+B08113_056E,
  WorkatHome_LEP_MOE = sqrt(B08113_053M^2+B08113_056M^2)
) %>%
  select(Geo,Year,
         DriveAlone_NonLEP,DriveAlone_NonLEP_MOE,DriveAlone_LEP,DriveAlone_LEP_MOE,
         Carpool_NonLEP,Carpool_NonLEP_MOE,Carpool_LEP,Carpool_LEP_MOE, 
         PublicTransport_NonLEP,PublicTransport_NonLEP_MOE,PublicTransport_LEP,PublicTransport_LEP_MOE,
         Walk_NonLEP, Walk_NonLEP_MOE,Walk_LEP,Walk_LEP_MOE, 
         TaxiBikeOther_NonLEP,TaxiBikeOther_NonLEP_MOE,TaxiBikeOther_LEP,TaxiBikeOther_LEP_MOE,
         WorkatHome_NonLEP,WorkatHome_NonLEP_MOE,WorkatHome_LEP,WorkatHome_LEP_MOE)


# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(LEP_mode, file = "LEP/LEP_mode_2018.csv")
      