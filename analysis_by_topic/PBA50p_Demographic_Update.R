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

acs_year    =  2022  # Set ACS year
acs_product = "acs5" # Set ACS 1- or 5-year dataset

## Geography for Bay Area counties and California state

baycounties=c("01","13","41","55","75","81","85","95","97")
statenumber="06"

## Assign variables

# Rent burden (Gross Rent as a Percentage of Household Income in the Past 12 Months)

rent_burden <- c(tot_rent_                     =    "B25070_002",  # Total renter occupied housing units
                 rent_30_35_                   =    "B25070_007",  # 30.0 to 34.9 percent
                 rent_35_40_                   =    "B25070_008",  # 35.0 to 39.9 percent
                 rent_40_50_                   =    "B25070_009",  # 40.0 to 49.9 percent
                 rent_50p_                     =    "B25070_010")  # 50.0 percent or more

# Families with low income (Ratio of Income to Poverty Level in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children)

low_income_families <- c(married_under_1.30_   =    "B17022_004",  # Married with children, income < 1.30 ratio to poverty
                         male_under_1.30_      =    "B17022_011",  # Male householder with children, income < 1.30 ratio to poverty
                         female_under_1.30_    =    "B17022_017",  # Female householder with children, income < 1.30 ratio to poverty
                         married_1.30_1.49_    =    "B17022_024",  # Married with children, income 1.30-1.49 ratio to poverty
                         male_1.30_1.49_       =    "B17022_031",  # Male householder with children, income 1.30-1.49 ratio to poverty
                         female_1.30_1.49_     =    "B17022_037",  # Female householder with children, income 1.30-1.49 ratio to poverty
                         married_1.50_1.84_    =    "B17022_044",  # Married with children, income 1.50-1.84 ratio to poverty
                         male_1.50_1.84_       =    "B17022_051",  # Male householder with children, income 1.50-1.84 ratio to poverty
                         female_1.50_1.84_     =    "B17022_057",  # Female householder with children, income 1.50-1.84 ratio to poverty
                         married_1.85p_        =    "B17022_064",  # Married with children, income > 1.85 ratio to poverty
                         male_1.85p_           =    "B17022_071",  # Male householder with children, income > 1.85 ratio to poverty
                         female_1.85p_         =    "B17022_077")  # Female householder with children, income > 1.85 ratio to poverty

# Median earnings by disability status (Median Earnings in the Past 12 Months (in ACS Year Inflation-Adjusted Dollars) by Disability Status by Sex for the Civilian Noninstitutionalized Population 16 Years and Over With Earnings)
# Include worker universe to calculate weighted median for full Bay Area from counties (Employment Status by Disability Status)

med_dis_earnings <- c(med_dis_earnings_        =    "B18140_002",  # Median earnings for workers with a disability
                      med_non_dis_earnings_    =    "B18140_005",  # Median earnings for workers with no disability
                      dis_worker_              =    "C18120_004",  # Workers with a disability
                      non_dis_worker_          =    "C18120_005")  # Workers with no disability

# Population with a disability (Age by Disability Status)
# First total then iterated by race/ethnicity
# Note that one should use Table B18101H for "white" because it omits Hispanic/Latino while Table B18101A includes Hispanic/Latino

# Total disability

disability <- c(tot_dis_universe_              =    "B18101_001",  # Disability universe - Civilian noninstitutionalized population
                male_dis_under5_               =    "B18101_004",  # Male under 5 with a disability
                male_dis_5_17_                 =    "B18101_007",  # Male 5 to 17 with a disability
                male_dis_18_34_                =    "B18101_010",  # Male 18 to 34 with a disability
                male_dis_35_64_                =    "B18101_013",  # Male 35 to 64 with a disability
                male_dis_65_74_                =    "B18101_016",  # Male 65 to 74 with a disability
                male_dis_75p_                  =    "B18101_019",  # Male 75 plus with a disability
                
                female_dis_under5_             =    "B18101_023",  # Female under 5 with a disability
                female_dis_5_17_               =    "B18101_026",  # Female 5 to 17 with a disability
                female_dis_18_34_              =    "B18101_029",  # Female 18 to 34 with a disability
                female_dis_35_64_              =    "B18101_032",  # Female 35 to 64 with a disability
                female_dis_65_74_              =    "B18101_035",  # Female 65 to 74 with a disability
                female_dis_75p_                =    "B18101_038",  # Female 75 plus with a disability

# White, not Hispanic/Latino disability

                white_dis_universe_            =    "B18101H_001", # Total white alone, not Hispanic/Latino civilian noninstitutionalized population
                white_dis_under18_             =    "B18101H_003", # White under 18 with a disability
                white_dis_18_64_               =    "B18101H_006", # White 18 to 64 with a disability
                white_dis_65p_                 =    "B18101H_009", # White 65 plus with a disability

# Black disability

                black_dis_universe_            =    "B18101B_001", # Total Black civilian noninstitutionalized population
                black_dis_under18_             =    "B18101B_003", # Black under 18 with a disability
                black_dis_18_64_               =    "B18101B_006", # Black 18 to 64 with a disability
                black_dis_65p_                 =    "B18101B_009", # Black 65 plus with a disability

# Asian disability

                asian_dis_universe_            =    "B18101D_001", # Total Asian civilian noninstitutionalized population
                asian_dis_under18_             =    "B18101D_003", # Asian under 18 with a disability
                asian_dis_18_64_               =    "B18101D_006", # Asian 18 to 64 with a disability
                asian_dis_65p_                 =    "B18101D_009", # Asian 65 plus with a disability

# Hispanic disability

                hispanic_dis_universe_         =    "B18101I_001", # Total Hispanic civilian noninstitutionalized population
                hispanic_dis_under18_          =    "B18101I_003", # Hispanic under 18 with a disability
                hispanic_dis_18_64_            =    "B18101I_006", # Hispanic 18 to 64 with a disability
                hispanic_dis_65p_              =    "B18101I_009", # Hispanic 65 plus with a disability

# American Indian and Alaska Native (AIAN) disability

                aian_dis_universe_             =    "B18101C_001", # Total AIAN civilian noninstitutionalized population
                aian_dis_under18_              =    "B18101C_003", # AIAN under 18 with a disability
                aian_dis_18_64_                =    "B18101C_006", # AIAN 18 to 64 with a disability
                aian_dis_65p_                  =    "B18101C_009", # AIAN 65 plus with a disability

# Native Hawaiian Pacific Islander (NHPI) disability

                nhpi_dis_universe_             =    "B18101E_001", # Total NHPI civilian noninstitutionalized population
                nhpi_dis_under18_              =    "B18101E_003", # NHPI under 18 with a disability
                nhpi_dis_18_64_                =    "B18101E_006", # NHPI 18 to 64 with a disability
                nhpi_dis_65p_                  =    "B18101E_009", # NHPI 65 plus with a disability

# Other race disability

                other_dis_universe_            =    "B18101F_001", # Total other civilian noninstitutionalized population
                other_dis_under18_             =    "B18101F_003", # Other under 18 with a disability
                other_dis_18_64_               =    "B18101F_006", # Other 18 to 64 with a disability
                other_dis_65p_                 =    "B18101F_009", # Other 65 plus with a disability

# Two or more race disability

                twoplus_dis_universe_          =    "B18101G_001", # Total twoplus civilian noninstitutionalized population
                twoplus_dis_under18_           =    "B18101G_003", # Twoplus under 18 with a disability
                twoplus_dis_18_64_             =    "B18101G_006", # Twoplus 18 to 64 with a disability
                twoplus_dis_65p_               =    "B18101G_009") # Twoplus 65 plus with a disability




# Combine all variables into single vector


total_acs_variables <- c(rent_burden,low_income_families,med_dis_earnings,disability)











# Import data into single wide dataframe, remove margin of error (any variable with "_M" suffix)

working_data <- get_acs(geography = "county",
                        variables = total_acs_variables,
                        state = statenumber,
                        county = baycounties,
                        year = acs_year,
                        survey = acs_product,
                        output = "wide") %>% 
  select(-c(ends_with("_M"),GEOID)) 
           



