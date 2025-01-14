library(dplyr)
library(tidycensus)

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
source_residence="B19013_ACS18_5YR"
source_work="B08521_ACS18_5YR"

# Census API Key
censuskey <- readLines("C:/Users/rmccoy/Documents/Vital Signs indicator pages/API.txt")
census_api_key(censuskey, install = TRUE, overwrite = TRUE)

# End variables to edit
################################################################

# get ACS data
ACS_variables <- c(Median_Income = "B19013_001",
                   Median_Earnings = "B08521_001",
                   Households = "B25009_001",
                   Workers = "B08604_001")

ACS_tract_raw <- get_acs(state = statenumber,
                         county = baycounties,
                         geography = "tract",
                         variables = ACS_variables,
                         year=ACS_year,
                         output="wide",
                         survey = ACS_product ) 

ACS_city_raw <- get_acs(state = statenumber,
                        geography = "place", variables = ACS_variables,
                        year=ACS_year,
                        output="wide",
                        survey = ACS_product ) %>%
  filter(GEOID %in% baycities)   # Only include places in the Bay Area 

# prepare for export
city_income_final_residence = ACS_city_raw %>% mutate(
  Id = paste0 ("1400000US06",GEOID),
  Id2 = GEOID,
  Year = ACS_year,
  City = sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Median_Household_Income = Median_IncomeE,
  Median_Household_Income_MOE = Median_IncomeM,
  Source = source_residence
) %>%
  select(Id, Id2,City,Year,Median_Household_Income,Median_Household_Income_MOE,Source)

city_income_final_work = ACS_city_raw %>% mutate(
  Id = paste0 ("1400000US06",GEOID),
  Id2 = GEOID,
  Year = ACS_year,
  City = sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Median_Worker_Earnings = Median_EarningsE,
  Median_Worker_Earnings_MOE = Median_EarningsM,
  Source = source_work
) %>%
  select(Id, Id2,City,Year,Median_Worker_Earnings,Median_Worker_Earnings_MOE,Source)

tract_income_final_residence = ACS_tract_raw %>% mutate(
  Id = paste0 ("1400000US06",GEOID),
  Id2 = GEOID,
  Year = ACS_year,
  County = sapply((strsplit(as.character(NAME),',')),function(x) x[2]),
  Tract = sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Median_Household_Income = Median_IncomeE,
  Median_Household_Income_MOE = Median_IncomeM,
  Source = source_residence
) %>%
  select(Id, Id2,County,Tract,Year,Median_Household_Income,Median_Household_Income_MOE,Source)


# Write out CSV 
setwd("C:/Users/rmccoy/Box/Vital Signs/1_Data/3_Economy")
write.csv(city_income_final_residence, file = "EC4_Income by Place of Residence/2018/residence_income_city_2018.csv")
write.csv(tract_income_final_residence, file = "EC4_Income by Place of Residence/2018/residence_income_tract_2018.csv")
write.csv(city_income_final_work, file = "EC5_Income by Place of Work/2018/workplace_earnings_city_2018.csv")
