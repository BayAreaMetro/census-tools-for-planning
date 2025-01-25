# PBA50p_Demographic_Update.R
# Import all the appropriate variables for PBA50+ demographic update
# Write out data into individual CSVs for updating relevant charts/tables
# Note that variables are assigned a "_" suffix for easier removal of MOE variables and clearer naming conventions

# Bring in libraries

suppressMessages(library(tidyverse))
library(tidycensus)

# Set file directories for input and output

USERPROFILE        <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
PBA50p_dir         <- file.path(USERPROFILE, "Box", "Plan Bay Area 2050+","Performance and Equity","Equity Analysis")

## Set ACS variables

acs_year    =  2022  # Set ACS year
acs_product = "acs5" # Set ACS 1- or 5-year dataset

## Geography for Bay Area counties/places, including place EQ that can be found here: "M:/Crosswalks/Census/PBA50+/Place_Geo_Classification_Plan_EQ.csv"

statenumber="06"

baycounties=c("01","13","41","55","75","81","85","95","97")

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

# Bayside cities, big three cities, inland/coastal/delta

bayside = c("0600562", "0600674", "0603092", "0605108", "0605164", "0606000", 
            "0608310", "0609066", "0610345", "0614736", "0616462", "0617610", 
            "0617918", "0620956", "0621796", "0622594", "0623168", "0625338", 
            "0626000", "0633000", "0633308", "0633798", "0640438", "0643280", 
            "0643294", "0644112", "0646870", "0647486", "0647710", "0647766", 
            "0648956", "0649670", "0650916", "0654806", "0655282", "0656938", 
            "0657288", "0658380", "0660102", "0660620", "0662980", "0664434", 
            "0665028", "0665070", "0668084", "0668252", "0668294", "0668364", 
            "0669084", "0670280", "0670364", "0673262", "0677000", "0678666", 
            "0681204", "0681666", "0686440")

big_three = c("0653000", "0667000", "0668000")

in_coast_delta = c("0601640", "0602252", "0605290", "0608142", "0609892", "0613882", 
                   "0614190", "0616000", "0616560", "0617988", "0619402", "0620018", 
                   "0623182", "0629504", "0631708", "0633056", "0639122", "0641992", 
                   "0646114", "0649187", "0649278", "0650258", "0652582", "0653070", 
                   "0654232", "0656784", "0657456", "0657764", "0657792", "0660984", 
                   "0662546", "0664140", "0668378", "0670098", "0670770", "0672646", 
                   "0675630", "0681554", "0683346", "0685922", "0686930")

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

# Senior tenure (Tenure by Age of Householder)

tenure <- c(owner_75_84                        =    "B25007_010",  # Owner aged 75-85
            owner_85p                          =    "B25007_011",  # Owner aged 85 plus
            renter_75_84                       =    "B25007_020",  # Renter aged 75-85
            renter_85p                         =    "B25007_021")  # Renter aged 85 plus
            
# Vehicles (Tenure by Vehicles Available by Age of Householder)
# Variables are structured by numvehicles_tenure_agecategory

# Renters

vehicles <- c(all_renter_all                   =    "B25045_011",  # All renter occupied housing units
              zero_renter_all                  =    "B25045_012",  # All zero-vehicle rented housing units
              
# Owners
              all_owner_all                    =    "B25045_002",  # All renter occupied housing units
              zero_owner_all                   =    "B25045_003",  # All zero-vehicle rented housing units

# 15 to 34
              zero_owner_15_34                 =    "B25045_004",  # Owner zero-vehicle aged 15-34
              onep_owner_15_34                 =    "B25045_008",  # Owner one-plus vehicle aged 15-34
              zero_renter_15_34                =    "B25045_013",  # Renter zero-vehicle aged 15-34
              onep_renter_15_34                =    "B25045_017",  # Renter one-plus vehicle aged 15-34

# 35 to 64
              zero_owner_35_64                 =    "B25045_005",  # Owner zero-vehicle aged 35-64
              onep_owner_35_64                 =    "B25045_009",  # Owner one-plus vehicle aged 35-64
              zero_renter_35_64                =    "B25045_014",  # Renter zero-vehicle aged 35-64
              onep_renter_35_64                =    "B25045_018",  # Renter one-plus vehicle aged 35-64

# 65 plus
              zero_owner_65p                   =    "B25045_006",  # Owner zero-vehicle aged 65 plus
              onep_owner_65p                   =    "B25045_010",  # Owner one-plus vehicle aged 65 plus
              zero_renter_65p                  =    "B25045_015",  # Renter zero-vehicle aged 65 plus
              onep_renter_65p                  =    "B25045_019")  # Renter one-plus vehicle aged 65 plus



# Combine all variables into single vector


total_acs_variables <- c(rent_burden,low_income_families,med_dis_earnings,disability,tenure,vehicles)











# Import data into single wide dataframe, remove margin of error (any variable with "_M" suffix)

working_county <- get_acs(geography = "county",
                        variables = total_acs_variables,
                        state = statenumber,
                        county = baycounties,
                        year = acs_year,
                        survey = acs_product,
                        output = "wide") %>% 
  select(-c(ends_with("_M"),GEOID)) 

working_place <- get_acs(geography = "place",
                          variables = total_acs_variables,
                          state = statenumber,
                          year = acs_year,
                          survey = acs_product,
                          output = "wide") %>% 
  select(-c(ends_with("_M"))) %>% 
  filter(GEOID %in% baycities)


