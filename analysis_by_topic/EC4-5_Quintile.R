# Import Libraries
library(dplyr)
library(tidycensus)

ACS_year=2018
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
source_residence="B19013_ACS18_1YR"
source_work="B08521_ACS18_1YR"
# 
#if 2013 or before metro data:
# metros = c("37980,47900,26420,31100,31080,16980,19100,35620,12060")


# Census API Key
censuskey = readLines("C:/Users/rmccoy/Documents/Vital Signs indicator pages/API.txt")
census_api_key(censuskey, install = TRUE, overwrite = TRUE)




# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."

ACS_variables <- c(Median_Income = "B19013_001",
                   Median_Earnings = "B08521_001",
                   Households = "B25009_001",
                   Workers = "B08604_001",
                   Household_Inc_Lowest = "B19080_001",
                   Household_Inc_2nd = "B19080_002",
                   Household_Inc_3rd = "B19080_003",
                   Household_Inc_4th = "B19080_004",
                   Household_Inc_Top = "B19080_005")

ACS_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )

ACS_metro_raw <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                         variables = ACS_variables,
                         year=ACS_year,
                         output="wide",
                         survey = ACS_product ) %>% 
  filter(GEOID %in% metros) 

# For counties - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_county <- ACS_county_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  Median_Income = as.numeric(Median_IncomeE),
  Median_Income_MOE = as.numeric (Median_IncomeM),
  Median_Earnings = as.numeric(Median_EarningsE),
  Median_Earnings_MOE = as.numeric(Median_EarningsM),
  Households = as.numeric(HouseholdsE),
  Workers = as.numeric(WorkersE),
  Income_x_Households = Median_Income*Households,
  Earnings_x_Workers = Median_Earnings*Workers,
  Residence_Source = source_residence,
  Work_Source = source_work,
  Percentile_20 = as.numeric(Household_Inc_LowestE),
  Percentile_40 = as.numeric(Household_Inc_2ndE),
  Percentile_60 = as.numeric(Household_Inc_3rdE),
  Percentile_80 = as.numeric(Household_Inc_4thE),
  Percentile_top = as.numeric(Household_Inc_TopE)
)



# Create separate county files for income and earnings

residence_income_county <- income_county %>% 
  select(Geo, Year, Median_Income, Median_Income_MOE, Percentile_20, Percentile_40, Percentile_60, Percentile_80, Percentile_top, Residence_Source) %>% 
  rename(Residence_Geo=Geo, Source=Residence_Source)

workplace_earnings_county <- income_county %>% 
  select(Geo, Year, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo, Source=Work_Source)

# For metros - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_metro1 <- ACS_metro_raw %>% mutate (
  Geo=ifelse(NAME=="Bay Area","Bay Area",paste(sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),"MSA")),
  Year=ACS_year,
  Median_Income = as.numeric(Median_IncomeE),
  Median_Income_MOE = as.numeric (Median_IncomeM),
  Median_Earnings = as.numeric(Median_EarningsE),
  Median_Earnings_MOE = as.numeric(Median_EarningsM),
  Residence_Source = source_residence,
  Work_Source = source_work,
  Percentile_20 = as.numeric(Household_Inc_LowestE),
  Percentile_40 = as.numeric(Household_Inc_2ndE),
  Percentile_60 = as.numeric(Household_Inc_3rdE),
  Percentile_80 = as.numeric(Household_Inc_4thE),
  Percentile_top = as.numeric(Household_Inc_TopE)
)

residence_income_metro <- income_metro1 %>% 
  select(Geo, Year, Median_Income, Median_Income_MOE, Percentile_20, Percentile_40, Percentile_60, Percentile_80, Percentile_top, Residence_Source) %>%
  rename(Residence_Geo=Geo,Source=Residence_Source)

# calculate stats for Bay Area
bay_median_income = round(sum(income_county$Income_x_Households)/sum(income_county$Households))
bay_20_pct = round(sum(income_county$Percentile_20*income_county$Households)/sum(income_county$Households))
bay_40_pct = round(sum(income_county$Percentile_40*income_county$Households)/sum(income_county$Households))
bay_60_pct = round(sum(income_county$Percentile_60*income_county$Households)/sum(income_county$Households))
bay_80_pct = round(sum(income_county$Percentile_80*income_county$Households)/sum(income_county$Households))
bay_top_pct = round(sum(income_county$Percentile_top*income_county$Households)/sum(income_county$Households))
bay_median_earnings = round(sum(income_county$Earnings_x_Workers)/sum(income_county$Workers))

# join to the rest of metros
residence_income_metro <- rbind(residence_income_metro,c("Bay Area",ACS_year, bay_median_income, "NA", bay_20_pct, bay_40_pct,
                                                         bay_60_pct,bay_80_pct,bay_top_pct,source_residence))


workplace_earnings_metro <- income_metro1 %>% 
  select(Geo, Year, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo,Source=Work_Source)

workplace_earnings_metro <- rbind(workplace_earnings_metro,c("Bay Area",ACS_year,bay_median_earnings, "NA", source_work))

# Write out CSV 
setwd("C:/Users/rmccoy/Box/Vital Signs/1_Data/3_Economy")
write.csv(residence_income_metro, file = "EC4_Income by Place of Residence/2018/residence_income_metro_quintiles_2018.csv")
write.csv(residence_income_county, file = "EC4_Income by Place of Residence/2018/residence_income_county_quintiles_2018.csv")
write.csv(workplace_earnings_metro, file = "EC5_Income by Place of Work/2018/workplace_earnings_metro_2017.csv")
write.csv(workplace_earnings_county, file = "EC5_Income by Place of Work/2018/workplace_earnings_county_2018.csv")