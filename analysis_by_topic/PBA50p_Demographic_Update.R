# PBA50p_Demographic_Update.R
# Import all the appropriate variables for PBA50+ demographic update
# Write out data into individual CSVs for updating relevant charts/tables

# Bring in libraries

suppressMessages(library(tidyverse))
library(tidycensus)

# Set file directories for input and output

USERPROFILE        <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
PBA50p_dir         <- file.path(USERPROFILE, "Box", "Plan Bay Area 2050+","Performance and Equity","Equity Analysis")

## Set ACS variables

ACS_year    = "2022" # Set ACS year
ACS_product = "acs5" # Set ACS 1- or 5-year dataset

## Geography for Bay Area counties and California state

baycounties=c("01","13","41","55","75","81","85","95","97")
statenumber="06"

## Assign variables

# Rent burden (Gross Rent as a Percentage of Household Income in the Past 12 Months)

rent_burden <- c(tot_rent_                     =    "B25070_002", # Total renter occupied housing units
                 rent_30_35_                   =    "B25070_007", # 30.0 to 34.9 percent
                 rent_35_40_                   =    "B25070_008", # 35.0 to 39.9 percent
                 rent_40_50_                   =    "B25070_009", # 40.0 to 49.9 percent
                 rent_50p_                     =    "B25070_010") # 50.0 percent or more

# Families with low income (Ratio of Income to Poverty Level in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children)

low_income_families <- c(married_under_1.30_   =    "B17022_004", # Married with children, income < 1.30 ratio to poverty
                         male_under_1.30_      =    "B17022_011", # Male householder with children, income < 1.30 ratio to poverty
                         female_under_1.30_    =    "B17022_017", # Female householder with children, income < 1.30 ratio to poverty
                         married_1.30_1.49_    =    "B17022_024", # Married with children, income 1.30-1.49 ratio to poverty
                         male_1.30_1.49_       =    "B17022_031", # Male householder with children, income 1.30-1.49 ratio to poverty
                         female_1.30_1.49_     =    "B17022_037", # Female householder with children, income 1.30-1.49 ratio to poverty
                         married_1.50_1.84_    =    "B17022_044", # Married with children, income 1.50-1.84 ratio to poverty
                         male_1.50_1.84_       =    "B17022_051", # Male householder with children, income 1.50-1.84 ratio to poverty
                         female_1.50_1.84_     =    "B17022_057", # Female householder with children, income 1.50-1.84 ratio to poverty
                         married_1.85p_        =    "B17022_064", # Married with children, income > 1.85 ratio to poverty
                         male_1.85p_           =    "B17022_071", # Male householder with children, income > 1.85 ratio to poverty
                         female_1.85p_         =    "B17022_077") # Female householder with children, income > 1.85 ratio to poverty

# Median earnings by disability status (Median Earnings in the Past 12 Months (in ACS Year Inflation-Adjusted Dollars) by Disability Status by Sex for the Civilian Noninstitutionalized Population 16 Years and Over With Earnings)
# Include worker universe to calculate weighted median for full Bay Area from counties (Employment Status by Disability Status)

med_dis_earnings <- c(med_dis_earnings_        =    "B18140_002", # Median earnings for workers with a disability
                      med_non_dis_earnings_    =    "B18140_005", # Median earnings for workers with no disability
                      dis_worker_              =    "C18120_004", # Workers with a disability
                      non_dis_worker_          =    "C18120_005") # Workers with no disability

# Population with a disability (Age by Disability Status)
# First total then iterated by race/ethnicity
# Note that one should use Table B18101H for "white" because it omits Hispanic/Latino while Table B18101A includes it

disability <- c(tot_dis_universe               = )


