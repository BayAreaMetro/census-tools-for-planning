#B25074
#Also includes longitudinal data

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
v13_acs5 <- load_variables(2013, "acs5", cache = TRUE)

#v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)
voo <- load_variables(2000, "sf3", cache = TRUE)
#v9o <- load_variables(1990, "sf3", cache = TRUE)

#view(voo)

ACS18_variables <- c(Lessthan10k_Under20p="B25074_003",
                   Lessthan10k_20to249p="B25074_004",
                   Lessthan10k_25to299p="B25074_005",
                   Lessthan10k_30to349p="B25074_006",
                   Lessthan10k_35to399p="B25074_007",
                   Lessthan10k_40to499p="B25074_008",
                   Lessthan10k_Over50p="B25074_009",
                   Lessthan10k_Notcomputed="B25074_010",
                   t10kto19k_Under20p="B25074_012",
                   t10kto19k_20to249p="B25074_013",
                   t10kto19k_25to299p="B25074_014",
                   t10kto19k_30to349p="B25074_015",
                   t10kto19k_35to399p="B25074_016",
                   t10kto19k_40to499p="B25074_017",
                   t10kto19k_Over50p="B25074_018",
                   t10kto19k_Notcomputed="B25074_019",
                   t20kto34k_Under20p="B25074_021",
                   t20kto34k_20to249p="B25074_022",
                   t20kto34k_25to299p="B25074_023",
                   t20kto34k_30to349p="B25074_024",
                   t20kto34k_35to399p="B25074_025",
                   t20kto34k_40to499p="B25074_026",
                   t20kto34k_Over50p="B25074_027",
                   t20kto34k_Notcomputed="B25074_028",
                   t35kto49k_Under20p="B25074_030",
                   t35kto49k_20to249p="B25074_031",
                   t35kto49k_25to299p="B25074_032",
                   t35kto49k_30to349p="B25074_033",
                   t35kto49k_35to399p="B25074_034",
                   t35kto49k_40to499p="B25074_035",
                   t35kto49k_Over50p="B25074_036",
                   t35kto49k_Notcomputed="B25074_037",
                   t50kto74k_Under20p="B25074_039",
                   t50kto74k_20to249p="B25074_040",
                   t50kto74k_25to299p="B25074_041",
                   t50kto74k_30to349p="B25074_042",
                   t50kto74k_35to399p="B25074_043",
                   t50kto74k_40to499p="B25074_044",
                   t50kto74k_Over50p="B25074_045",
                   t50kto74k_Notcomputed="B25074_046",
                   t75kto99k_Under20p="B25074_048",
                   t75kto99k_20to249p="B25074_049",
                   t75kto99k_25to299p="B25074_050",
                   t75kto99k_30to349p="B25074_051",
                   t75kto99k_35to399p="B25074_052",
                   t75kto99k_40to499p="B25074_053",
                   t75kto99k_Over50p="B25074_054",
                   t75kto99k_Notcomputed="B25074_055",
                   Over100k_Under20p="B25074_057",
                   Over100k_20to249p="B25074_058",
                   Over100k_25to299p="B25074_059",
                   Over100k_30to349p="B25074_060",
                   Over100k_35to399p="B25074_061",
                   Over100k_40to499p="B25074_062",
                   Over100k_Over50p="B25074_063",
                   Over100k_Notcomputed="B25074_063")


Cen1990_variables <- c(Lessthan10p="FBA002",
                       p10to14_9="FBA003")
                       

ACS18_county_raw <- get_acs(geography = "county", variables = ACS_variables,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product)

ACS18_county_1yr_raw <- get_acs(geography = "county", variables = ACS_variables,
                            state = statenumber, county=baycounties,
                            year=ACS_year,
                            output="tidy",
                            survey = "acs1")

Rentburden18_county <- ACS_county_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  RentBurden= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
  ) %>% rename(Population = estimate) %>% mutate(
    Income = replace(Income, Income == 'Lessthan10k', '<$10k'))%>% mutate (
    Income = replace(Income, Income == 't10kto19k', '<$35k'))%>% mutate (
    Income = replace(Income, Income == 't20kto34k', '<$35k'))%>% mutate (
    Income = replace(Income, Income == 't35kto49k', '$35-$49.9k'))%>% mutate (
    Income = replace(Income, Income == 't50kto74k', '$50-$74.9k'))%>% mutate (
    Income = replace(Income, Income == 't75kto99k', '$75-$99k'))%>% mutate (
    Income = replace(Income, Income == 'Over100k', '>$100k'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == 'Under20p', 'Less than 20% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == '20to249p', '20%-34% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == '25to299p', '20%-34% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == '30to349p', '20%-34% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == '35to399p', 'At least 35% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == '40to499p', 'At least 35% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == 'Over50p', 'At least 35% of income'))%>% mutate (
    RentBurden = replace(RentBurden, RentBurden == 'Notcomputed', 'Not computed'))%>% select(Geo,GEOID, Year,Income,RentBurden,Population)

Rentburden18_county_1yr <- ACS18_county_1yr_raw %>% mutate(
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Income= sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  RentBurden= sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
) %>% rename(Population = estimate) %>% mutate(
  Income = replace(Income, Income == 'Lessthan10k', '<$10k'))%>% mutate (
  Income = replace(Income, Income == 't10kto19k', '<$35k'))%>% mutate (
  Income = replace(Income, Income == 't20kto34k', '<$35k'))%>% mutate (
  Income = replace(Income, Income == 't35kto49k', '$35-$49.9k'))%>% mutate (
  Income = replace(Income, Income == 't50kto74k', '$50-$74.9k'))%>% mutate (
  Income = replace(Income, Income == 't75kto99k', '$75-$99k'))%>% mutate (
  Income = replace(Income, Income == 'Over100k', '>$100k'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == 'Under20p', 'Less than 20% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == '20to249p', '20%-34% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == '25to299p', '20%-34% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == '30to349p', '20%-34% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == '35to399p', 'At least 35% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == '40to499p', 'At least 35% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == 'Over50p', 'At least 35% of income'))%>% mutate (
  RentBurden = replace(RentBurden, RentBurden == 'Notcomputed', 'Not computed'))%>% select(Geo,GEOID, Year,Income,RentBurden,Population)
# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(Rentburden18_county_1yr, file = "Cost_burdened/rent_burden_income_2018_1yr.csv")


#2013 NOTE above table not available in 2013 INSTEAD use B25070
ACS13_variables <- c(Lessthan10p="B25070_002",
                     p10to14_9="B25070_003",
                     p15to19_9="B25070_004",
                     p20to24_9="B25070_005",
                     p25to29_9="B25070_006",
                     p30to34_9="B25070_007",
                     p35to39_9="B25070_008",
                     p40to49_9="B25070_009",
                     p50plus="B25070_010",
                     notcomputed="B25070_011")

ACS13_county_raw <- get_acs(geography = "county", variables = ACS13_variables,
                            state = statenumber, county=baycounties,
                            year=2013,
                            output="tidy",
                            survey = ACS_product )

#2000 data
Cen2000_variables <- c(Lessthan10p="H069002",
                       p10to14_9="H069003",
                       p15to19_9="H069004",
                       p20to24_9="H069005",
                       p25to29_9="H069006",
                       p30to34_9="H069007",
                       p35to39_9="H069008",
                       p40to49_9="H069009",
                       p50plus="H069010")


Cen2000_county_raw <- get_decennial(geography = "county", variables = Cen2000_variables,
                                    state = statenumber, county=baycounties,
                                    year=2000,
                                    output="tidy")
Cen2000_county_raw <- get_decennial(geography = "county",table ="H069",
                                    state = statenumber, county=baycounties,
                                    year=2000,
                                    output="tidy")
#1990 data


