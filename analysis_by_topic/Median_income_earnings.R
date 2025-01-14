# median-income-earnings-county-metro-one-year.R
# One year ACS calculations for median income and earnings
# November 2, 2018
# SI


# Bring in appropriate packages and install Census key

suppressMessages(library(dplyr))
library(tidycensus)

censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)


# Import ACS library for variable inspection

ACS_table <- load_variables(year=2016, dataset="acs5", cache=TRUE)

# Set up census variables for API

ACS_year <- 2016
ACS_product <- "acs1"
baycounties <- c("01","13","41","55","75","81","85","95","97")
metros <- c("37980","47900","26420","33100","31080","16980","19100","35620","12060") # if 2014 or after metro data
statenumber="06"

# Create documentation names for ACS table and year, later appended to file

source_residence="B19013_ACS16_1YR"
source_work="B08521_ACS16_1YR"

# Set up paths for saving to Box Drive

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_residence        <- file.path(USERPROFILE, "Box", "1_Data", "3_Economy", "EC4_Income by Place of Residence", ACS_year)
BOX_work             <- file.path(USERPROFILE, "Box", "1_Data", "3_Economy", "EC5_Income by Place of Work", ACS_year)


# Populate ACS variables with needed table cells
# Import ACS API data for income and earnings for county and metro of work, respectively.

ACS_variables <- c(Median_Income = "B19013_001",
                   Median_Earnings = "B08521_001",
                   Households = "B25009_001",
                   Workers = "B08604_001")

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
  filter(GEOID %in% metros)   # Only include metros of interest


# For counties - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_county <- ACS_county_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Households = HouseholdsE,
  Workers = WorkersE,
  Median_Income = Median_IncomeE,
  Median_Income_MOE = Median_IncomeM,
  Median_Earnings = Median_EarningsE,
  Median_Earnings_MOE = Median_EarningsM,
  Income_x_Households = Median_IncomeE*HouseholdsE,
  Earnings_x_Workers = Median_EarningsE*WorkersE,
  Residence_Source = source_residence,
  Work_Source = source_work
) 


# Create separate county files for income and earnings

residence_income_county <- income_county %>% 
  select(Geo, Median_Income, Median_Income_MOE, Residence_Source) %>% 
  rename(Residence_Geo=Geo, Source=Residence_Source)

workplace_earnings_county <- income_county %>% 
  select(Geo, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo, Source=Work_Source)

# For metros - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_metro <- ACS_metro_raw %>% mutate (
  Geo=(paste(sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),"MSA")), # Reformat name
  Year=ACS_year,
  Median_Income = Median_IncomeE,
  Median_Income_MOE = Median_IncomeM,
  Median_Earnings = Median_EarningsE,
  Median_Earnings_MOE = Median_EarningsM,
  Residence_Source = source_residence,
  Work_Source = source_work
)

# Calculate a Bay Area weighted average of household income/worker earnings to compare with other metros

bay_median_income = round(sum(income_county$Income_x_Households)/sum(income_county$Households))
bay_median_earnings = round(sum(income_county$Earnings_x_Workers)/sum(income_county$Workers))


residence_income_metro <- income_metro %>% 
  select(Geo, Median_Income, Median_Income_MOE, Residence_Source) %>%
  rename(Residence_Geo=Geo,Source=Residence_Source)

residence_income_metro <- rbind(residence_income_metro,c("Bay Area", bay_median_income, "NA", source_residence))

workplace_earnings_metro <- income_metro %>% 
  select(Geo, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo,Source=Work_Source)

workplace_earnings_metro <- rbind(workplace_earnings_metro, c("Bay Area", bay_median_earnings, "NA", source_work))

# Write out CSV 
write.csv(residence_income_county, paste0(BOX_residence, "/","1Year_Residence_County_Income.csv"), row.names = FALSE, quote = T)
write.csv(residence_income_metro, paste0(BOX_residence, "/", "1Year_Residence_Metro_Income.csv"), row.names = FALSE, quote = T)
write.csv(workplace_earnings_county, paste0(BOX_work, "/", "1Year_Workplace_County_Earnings.csv"), row.names = FALSE, quote = T)
write.csv(workplace_earnings_metro, paste0(BOX_work, "/", "1Year_Workplace_Metro_Earnings.csv"), row.names = FALSE, quote = T)