# Import Libraries
library(dplyr)
library(tidycensus)
library(httr)
library(jsonlite)
library(lubridate)

# Create documentation names for ACS table and year, later appended to file

source_average="B19013_ACS18_1YR"
source_black="B19013B_ACS18_1YR"
source_asian="B19013D-E_ACS18_1YR"
source_latino="B19013I_ACS18_1YR"
source_singleparent="B19126_ACS18_1YR"
source_senior="B19049_ACS18_1YR"
#note disabilities is earnings not income
source_disabilities="B18140_ACS18_1YR"

#set up Census API  
censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")


ACS_year=2018
#note: add additional years for changes
ACS_product="acs1"
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
#v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)

#by characteristics of householder unless otherwise specified
ACS_variables <- c(Median_Total= "B19013_001",
                   Median_Black = "B19013B_001",
                   Median_AsianOnly = "B19013D_001",
                   Median_HPI = "B19013E_001",
                   Median_Latino = "B19013I_001",
                   Median_WhiteOnly = "B19013H_001",
    #single parents is by families not households 
                   Median_SingleFather = "B19126_008",
                   Median_SingleMother = "B19126_010",
                   Median_Senior = "B19049_005",
    #Disabilities Income is by civilian non-institutionalized pop over 18 with earnings
                   Median_Disabilities = "B18140_002",
                   Median_NoDisabilities = "B18140_005",
    #The tables used for household by race/ethnicity have identical results where results
    #are available. Pops with small pops in some counties like HPI and Black are N/A for 
    #B25032X. White non-Hispanic and Latino are only in the B25032X series
                   Total_Households="B25006_001",
                   Black_Households="B25006_003",
                   Asian_Households="B25006_005",
                   HPI_Households="B25006_006",
                   Latino_Households="B25032I_001",
                   White_Households="B25032H_001",
                   SingleFather_Families="B11003_010",
                   SingleMother_Families="B11003_016",
                   Senior_Households="B19037_053",
    #Disabilities Pop is by civilian non-institutionalized pop 18-64. Variables compared Employed
                   Disabilities_Pop="C18120_004",
                   NoDisabilities_Pop="C18120_005")

#Call VitalSigns data 
#VS_Income <- fromJSON("https://data.bayareametro.gov/id/hp78-6nm2.json")
#head(VS_Income)

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


income_county <- ACS_county_raw %>% 
  replace(is.na(.),0) %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Total_Households = Total_HouseholdsE,
  Black_Households = Black_HouseholdsE,
  API_Households = Asian_HouseholdsE+HPI_HouseholdsE,
  Asian_Households = Asian_HouseholdsE,
  Latino_Households = Latino_HouseholdsE,
  White_Households = White_HouseholdsE,
  SingleFather_Families = SingleFather_FamiliesE,
  SingleMother_Families = SingleMother_FamiliesE,
  Senior_Households = Senior_HouseholdsE,
  Disability_EmployedPop = Disabilities_PopE,
  NoDisability_EmployedPop = NoDisabilities_PopE,
  Median_Income = Median_TotalE,
  Median_Income_MOE = Median_TotalM,
  Median_Black = Median_BlackE,
  Median_Black_MOE = Median_BlackM,
  Median_API = (Median_AsianOnlyE*Asian_HouseholdsE+Median_HPIE*HPI_HouseholdsE)/(Asian_HouseholdsE+HPI_HouseholdsE),
  #Median API MOE is complex...come back to it
  Median_Asian = Median_AsianOnlyE,
  Median_Asian_MOE = Median_AsianOnlyM,
  Median_Latino = Median_LatinoE,
  Median_Latino_MOE = Median_LatinoM,
  Median_WhiteOnly = Median_WhiteOnlyE,
  Median_WhiteOnly_MOE = Median_WhiteOnlyM,
  Median_SingleFather = Median_SingleFatherE,
  Median_SingleFather_MOE = Median_SingleFatherM,
  Median_SingleMother = Median_SingleMotherE,
  Median_SingleMother_MOE = Median_SingleMotherM,
  Median_Senior = Median_SeniorE,
  Median_Senior_MOE = Median_SeniorM,
  Median_Earnings_Disabilities = Median_DisabilitiesE,
  Median_Earnings__DisabilitiesMOE = Median_DisabilitiesM,
  Median_Earnings_NoDisabilities = Median_NoDisabilitiesE,
  Median_Earnings__NoDisabilitiesMOE = Median_NoDisabilitiesM, 
  Total_Income_x_Households = Median_TotalE*Total_HouseholdsE,
  Black_Income_x_Households = Median_BlackE*Black_HouseholdsE,
  API_Income_x_Households = Median_AsianOnlyE*Asian_HouseholdsE+Median_HPIE*HPI_HouseholdsE,
  Asian_Income_x_Households = Median_AsianOnlyE*Asian_HouseholdsE,
  Latino_Income_x_Households = Median_LatinoE*Latino_HouseholdsE,
  White_Income_x_Households = Median_WhiteOnlyE*White_HouseholdsE,
  SingleFather_Income_x_Families = Median_SingleFatherE*SingleFather_FamiliesE,
  SingleMother_Income_x_Families = Median_SingleMotherE*SingleMother_FamiliesE,
  Senior_Income_x_Households = Median_SeniorE*Senior_HouseholdsE,
  Disabilities_Earnings_x_Households = Median_DisabilitiesE*Disabilities_PopE,
  NoDisabilities_Earnings_x_Households = Median_NoDisabilitiesE*NoDisabilities_PopE) 

#For city
income_city <- ACS_city_raw %>% 
  replace(is.na(.),0) %>% mutate(
    Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
    Year=ACS_year,
    Total_Households = Total_HouseholdsE,
    Black_Households = Black_HouseholdsE,
    API_Households = Asian_HouseholdsE+HPI_HouseholdsE,
    Asian_Households = Asian_HouseholdsE,
    Latino_Households = Latino_HouseholdsE,
    White_Households = White_HouseholdsE,
    SingleFather_Families = SingleFather_FamiliesE,
    SingleMother_Families = SingleMother_FamiliesE,
    Senior_Households = Senior_HouseholdsE,
    Disability_EmployedPop = Disabilities_PopE,
    NoDisability_EmployedPop = NoDisabilities_PopE,
    Median_Income = Median_TotalE,
    Median_Income_MOE = Median_TotalM,
    Median_Black = Median_BlackE,
    Median_Black_MOE = Median_BlackM,
    Median_API = (Median_AsianOnlyE*Asian_HouseholdsE+Median_HPIE*HPI_HouseholdsE)/(Asian_HouseholdsE+HPI_HouseholdsE),
    #Median API MOE is complex...come back to it
    Median_Asian = Median_AsianOnlyE,
    Median_Asian_MOE = Median_AsianOnlyM,
    Median_Latino = Median_LatinoE,
    Median_Latino_MOE = Median_LatinoM,
    Median_WhiteOnly = Median_WhiteOnlyE,
    Median_WhiteOnly_MOE = Median_WhiteOnlyM,
    Median_SingleFather = Median_SingleFatherE,
    Median_SingleFather_MOE = Median_SingleFatherM,
    Median_SingleMother = Median_SingleMotherE,
    Median_SingleMother_MOE = Median_SingleMotherM,
    Median_Senior = Median_SeniorE,
    Median_Senior_MOE = Median_SeniorM,
    Median_Earnings_Disabilities = Median_DisabilitiesE,
    Median_Earnings__DisabilitiesMOE = Median_DisabilitiesM,
    Median_Earnings_NoDisabilities = Median_NoDisabilitiesE,
    Median_Earnings__NoDisabilitiesMOE = Median_NoDisabilitiesM, 
    Total_Income_x_Households = Median_TotalE*Total_HouseholdsE,
    Black_Income_x_Households = Median_BlackE*Black_HouseholdsE,
    API_Income_x_Households = Median_AsianOnlyE*Asian_HouseholdsE+Median_HPIE*HPI_HouseholdsE,
    Asian_Income_x_Households = Median_AsianOnlyE*Asian_HouseholdsE,
    Latino_Income_x_Households = Median_LatinoE*Latino_HouseholdsE,
    White_Income_x_Households = Median_WhiteOnlyE*White_HouseholdsE,
    SingleFather_Income_x_Families = Median_SingleFatherE*SingleFather_FamiliesE,
    SingleMother_Income_x_Families = Median_SingleMotherE*SingleMother_FamiliesE,
    Senior_Income_x_Households = Median_SeniorE*Senior_HouseholdsE,
    Disabilities_Earnings_x_Households = Median_DisabilitiesE*Disabilities_PopE,
    NoDisabilities_Earnings_x_Households = Median_NoDisabilitiesE*NoDisabilities_PopE) 

#Bay Area average for all pops
bay_median_income = round(sum(income_county$Total_Income_x_Households)/sum(income_county$Total_Households))
bay_median_income_city = round(sum(income_city$Total_Income_x_Households)/sum(income_city$Total_Households))
bay_black_income = round(sum(income_county$Black_Income_x_Households)/sum(income_county$Black_Households))
bay_API_income = round(sum(income_county$API_Income_x_Households)/sum(income_county$API_Households))
bay_Asian_income = round(sum(income_county$Asian_Income_x_Households)/sum(income_county$Asian_Households))
bay_latino_income = round(sum(income_county$Latino_Income_x_Households)/sum(income_county$Latino_Households))
bay_white_income = round(sum(income_county$White_Income_x_Households)/sum(income_county$White_Households))
bay_singlefather_income = round(sum(income_county$SingleFather_Income_x_Families)/sum(income_county$SingleFather_Families))
bay_singlemother_income = round(sum(income_county$SingleMother_Income_x_Families)/sum(income_county$SingleMother_Families))
bay_senior_income = round(sum(income_county$Senior_Income_x_Households)/sum(income_county$Senior_Households))
bay_disabilities_income = round(sum(income_county$Disabilities_Earnings_x_Households)/sum(income_county$Disability_EmployedPop))
bay_nodisabilities_income = round(sum(income_county$NoDisabilities_Earnings_x_Households)/sum(income_county$NoDisability_EmployedPop))

#select for printing
CoCpop_income <- income_county %>% 
  select(Geo, Year,
         Total_Households,
         Median_Income,
         Black_Households,
         Median_Black,
         Asian_Households,
         Median_Asian,
         Median_API,
         Latino_Households,
         Median_Latino, 
         White_Households,
         Median_WhiteOnly,
         SingleFather_Families,
         Median_SingleFather,
         SingleMother_Families,
         Median_SingleMother,
         Senior_Households,
         Median_Senior,
         Disability_EmployedPop,
         Median_Earnings_Disabilities,
         NoDisability_EmployedPop,
         Median_Earnings_NoDisabilities,
  )

# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(CoCpop_income, file = "Overall/Median_Income/2018/residence_CoCPop_median_income_2018.csv")

#for improvement
#note HPI has huge MOE...never a huge populations compared to Asian but consider excluding
#add all other race categories, perhaps compare to white just to see how differs from bay area average
#Disabilities show employed vs unemployed vs not in labor force

