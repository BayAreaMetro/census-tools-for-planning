# PBA50p_Demographic_Update.R
# Import all the appropriate variables for PBA50+ demographic update
# Write out data into individual CSVs for updating relevant charts/tables
# Note that variables are assigned a "_" suffix for easier removal of MOE variables and clearer naming conventions

# Bring in libraries

suppressMessages(library(tidyverse))
library(tidycensus)
library(sf)

# Eliminate scientific notation

options(scipen = 999)

# Set file directories for input and output

userprofile        <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
pba50p_dir         <- file.path(userprofile, "Box", "Plan Bay Area 2050+")
output             <- file.path(pba50p_dir,"Performance and Equity","Equity Analysis") 
hra_directory      <- file.path(pba50p_dir,"Regional Growth Framework","Growth Geographies","Final Blueprint Input data", "HRAs")

epc_2018_in        <- "M:/Crosswalks/Census/EPCs/equity_priority_communities_2020_acs2018_0.csv"
epc_2022_in        <- "M:/Crosswalks/Census/EPCs/equity_priority_communities_pba2050plus_acs2022_0.csv"
hra_in             <- file.path(hra_directory,"CTCAC_HRAs_2023.shp")

## Set ACS variables

#acs_year    =  2018  # Set ACS year
#acs_product = "acs5" # Set ACS 1- or 5-year dataset

## Geography for Bay Area counties/places, including place EQ that can be found here: "M:/Crosswalks/Census/PBA50+/Place_Geo_Classification_Plan_EQ.csv"
# Note that FIPS code changed for Moraga Town (Contra Costa County), Census 2000 to ACS years, from "0649194" to "0649187". Both values are included below. 

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
              "0681666","0683346","0685922","0686440","0686930","0649194")

# Bayside cities, big three cities, inland/coastal/delta
# Note that FIPS code changed for Moraga Town (Contra Costa County), Census 2000 to ACS years, from "0649194" to "0649187". Both values are included below.

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
                   "0675630", "0681554", "0683346", "0685922", "0686930","0649194")

# Bring in latest HRA shapefile and EPC data for 2018/2022 vintages and to get appropriate tract metadata for merging
# Make sure "Geographic.ID" reads as a character from the CSV to retain leading 0.
# HRAs include census tracts and block groups. Split those into respective files for downloading data (later to be merged)

hra_shapefile <- st_read(hra_in)
hra_metatada  <- st_drop_geometry(hra_shapefile) %>% 
  select(fipco, tract_geoi, blkgp_geoi,oppcat)
hra_tracts <- hra_metatada %>% filter(!is.na(tract_geoi))
hra_bgs <- hra_metatada %>% filter(!is.na(blkgp_geoi))

epc_2018      <- read.csv(epc_2018_in, colClasses = c("Geographic.ID"="character")) %>% 
  filter(PBA.2050.Equity.Priority.Community==1) %>% 
  select(Geographic.ID, County.FIPS, PBA.2050.Equity.Priority.Community)
epc_2022      <- read.csv(epc_2022_in,colClasses = c("Geographic.ID"="character")) %>% 
  filter(Equity.Priority.Community.PBA.2050.Plus==1) %>% 
  select(Geographic.ID, County.FIPS, 
         Equity.Priority.Community.PBA.2050.Plus)

## Assign variables

# Rent burden (Gross Rent as a Percentage of Household Income in the Past 12 Months)

rent_burden <- c(tot_rent_                     =    "B25070_001",  # Total renter occupied housing units
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


# Families by sex of householder

families <- c(married_family_                  =    "B11004_003",  # Married with children under 18
              male_householder_                =    "B11004_010",  # Male householder with children under 18
              female_householder_              =    "B11004_016")  # Married with children under 18


# Household income by race

med_inc_race <- c(med_inc_white_               =    "B19013H_001", # White alone, not Hispanic median income
                  med_inc_black_               =    "B19013B_001", # Black median income
                  med_inc_asian_               =    "B19013D_001", # Asian median income
                  med_inc_hispanic_            =    "B19013I_001") # Hispanic/Latino median income

# Race of householder (for weighting median household incomes to get regional average)
# Universe for this is household

hholder_race <- c(hholder_white_               =    "B22005H_001", # White alone, not Hispanic householder
                  hholder_black_               =    "B22005B_001", # Black median householder
                  hholder_asian_               =    "B22005D_001", # Asian median householder
                  hholder_hispanic_            =    "B22005I_001") # Hispanic/Latino householder

# Median earnings by disability status (Median Earnings in the Past 12 Months (in ACS Year Inflation-Adjusted Dollars) by Disability Status by Sex for the Civilian Noninstitutionalized Population 16 Years and Over With Earnings)
# Include worker universe to calculate weighted median for full Bay Area from counties (Employment Status by Disability Status)

med_dis_earnings <- c(med_dis_earnings_        =    "B18140_002",  # Median earnings for workers with a disability
                      med_non_dis_earnings_    =    "B18140_005",  # Median earnings for workers with no disability
                      dis_worker_              =    "C18120_004",  # Workers with a disability
                      non_dis_worker_          =    "C18120_005")  # Workers with no disability

# Population with a disability (Age by Disability Status)
# First total then iterated by race/ethnicity
# Note that one should use Table B18101H for "white" because it omits Hispanic/Latino while Table B18101A includes Hispanic/Latino

# Total disability, first with a disability and then without

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
                
                male_non_dis_under5_           =    "B18101_005",  # Male under 5 without a disability
                male_non_dis_5_17_             =    "B18101_008",  # Male 5 to 17 without a disability
                male_non_dis_18_34_            =    "B18101_011",  # Male 18 to 34 without a disability
                male_non_dis_35_64_            =    "B18101_014",  # Male 35 to 64 without a disability
                male_non_dis_65_74_            =    "B18101_017",  # Male 65 to 74 without a disability
                male_non_dis_75p_              =    "B18101_020",  # Male 75 plus without a disability
                
                female_non_dis_under5_         =    "B18101_024",  # Female under 5 without a disability
                female_non_dis_5_17_           =    "B18101_027",  # Female 5 to 17 without a disability
                female_non_dis_18_34_          =    "B18101_030",  # Female 18 to 34 without a disability
                female_non_dis_35_64_          =    "B18101_033",  # Female 35 to 64 without a disability
                female_non_dis_65_74_          =    "B18101_036",  # Female 65 to 74 without a disability
                female_non_dis_75p_            =    "B18101_039",  # Female 75 plus without a disability

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

tenure <- c(owner_75_84_                       =    "B25007_010",  # Owner aged 75-85
            owner_85p_                         =    "B25007_011",  # Owner aged 85 plus
            renter_75_84_                      =    "B25007_020",  # Renter aged 75-85
            renter_85p_                        =    "B25007_021")  # Renter aged 85 plus
            
# Vehicles (Tenure by Vehicles Available by Age of Householder)
# Variables are structured by numvehicles_tenure_agecategory, "all" is everyone in universe (zero and 1+ vehicle HHs combined)

# Renters

vehicles <- c(all_renter_all_                  =    "B25045_011",  # All renter occupied housing units
              zero_renter_all_                 =    "B25045_012",  # All zero-vehicle rented housing units
              
# Owners
              all_owner_all_                   =    "B25045_002",  # All renter occupied housing units
              zero_owner_all_                  =    "B25045_003",  # All zero-vehicle rented housing units

# 15 to 34
              zero_owner_15_34_                =    "B25045_004",  # Owner zero-vehicle aged 15-34
              onep_owner_15_34_                =    "B25045_008",  # Owner one-plus vehicle aged 15-34
              zero_renter_15_34_               =    "B25045_013",  # Renter zero-vehicle aged 15-34
              onep_renter_15_34_               =    "B25045_017",  # Renter one-plus vehicle aged 15-34

# 35 to 64
              zero_owner_35_64_                =    "B25045_005",  # Owner zero-vehicle aged 35-64
              onep_owner_35_64_                =    "B25045_009",  # Owner one-plus vehicle aged 35-64
              zero_renter_35_64_               =    "B25045_014",  # Renter zero-vehicle aged 35-64
              onep_renter_35_64_               =    "B25045_018",  # Renter one-plus vehicle aged 35-64

# 65 plus
              zero_owner_65p_                  =    "B25045_006",  # Owner zero-vehicle aged 65 plus
              onep_owner_65p_                  =    "B25045_010",  # Owner one-plus vehicle aged 65 plus
              zero_renter_65p_                 =    "B25045_015",  # Renter zero-vehicle aged 65 plus
              onep_renter_65p_                 =    "B25045_019")  # Renter one-plus vehicle aged 65 plus

# Limited-English proficiency (lep)
# First non-LEP cells then LEP cells

# Non-LEP, ages 5 to 17

non_lep <- c(english_only_5_17_                =    "B16004_003",  # Speaks English only, ages 5 to 17
            spanish_vwell_5_17_                =    "B16004_005",  # Speaks Spanish, English very well, ages 5 to 17
            indo_vwell_5_17_                   =    "B16004_010",  # Speaks Indo-European language, English very well, ages 5 to 17
            asian_vwell_5_17_                  =    "B16004_015",  # Speaks Asian/Pacific Islander language, English very well, ages 5 to 17
            other_vwell_5_17_                  =    "B16004_020",  # Speaks other language, English very well, ages 5 to 17

# Non-LEP, ages 18 to 64

            english_only_18_64_                =    "B16004_025",  # Speaks English only, ages 18 to 64
            spanish_vwell_18_64_               =    "B16004_027",  # Speaks Spanish, English very well, ages 18 to 64
            indo_vwell_18_64_                  =    "B16004_032",  # Speaks Indo-European language, English very well, ages 18 to 64
            asian_vwell_18_64_                 =    "B16004_037",  # Speaks Asian/Pacific Islander language, English very well, ages 18 to 64
            other_vwell_18_64_                 =    "B16004_042",  # Speaks Spanish, English very well, ages 18 to 64

# Non-LEP, ages 65-plus

            english_only_65p_                  =    "B16004_047",  # Speaks English only, ages 65 plus
            spanish_vwell_65p_                 =    "B16004_049",  # Speaks Spanish, English very well, ages 65 plus
            indo_vwell_65p_                    =    "B16004_054",  # Speaks Indo-European language, English very well, ages 65 plus
            asian_vwell_65p_                   =    "B16004_059",  # Speaks Asian/Pacific Islander language, English very well, ages 65 plus
            other_vwell_65p_                   =    "B16004_064")  # Speaks Spanish, English very well, ages 65 plus

# LEP, ages 5 to 17

lep <- c(spanish_well_5_17_                    =    "B16004_006",  # Speaks Spanish, English well, ages 5 to 17
         spanish_notwell_5_17_                 =    "B16004_007",  # Speaks Spanish, English not well, ages 5 to 17
         spanish_notatall_5_17_                =    "B16004_008",  # Speaks Spanish, English not at all, ages 5 to 17
         
         indo_well_5_17_                       =    "B16004_011",  # Speaks Indo-European language, English well, ages 5 to 17
         indo_notwell_5_17_                    =    "B16004_012",  # Speaks Indo-European language, English well, ages 5 to 17
         indo_notatall_5_17_                   =    "B16004_013",  # Speaks Indo-European language, English well, ages 5 to 17
         
         asian_well_5_17_                      =    "B16004_016",  # Speaks Asian/Pacific Islander language, English well, ages 5 to 17
         asian_notwell_5_17_                   =    "B16004_017",  # Speaks Asian/Pacific Islander language, English not well, ages 5 to 17
         asian_notatall_5_17_                  =    "B16004_018",  # Speaks Asian/Pacific Islander language, English not at all, ages 5 to 17
         
         other_well_5_17_                      =    "B16004_021",  # Speaks other language, English well, ages 5 to 17
         other_notwell_5_17_                   =    "B16004_022",  # Speaks other language, English well, ages 5 to 17
         other_notatall_5_17_                  =    "B16004_023",  # Speaks other language, English well, ages 5 to 17
         
# LEP, ages 18 to 64

         spanish_well_18_64_                   =    "B16004_028",  # Speaks Spanish, English well, ages 18 to 64
         spanish_notwell_18_64_                =    "B16004_029",  # Speaks Spanish, English not well, ages 18 to 64
         spanish_notatall_18_64_               =    "B16004_030",  # Speaks Spanish, English not at all, ages 18 to 64
        
         indo_well_18_64_                      =    "B16004_033",  # Speaks Indo-European language, English well, ages 18 to 64
         indo_notwell_18_64_                   =    "B16004_034",  # Speaks Indo-European language, English well, ages 18 to 64
         indo_notatall_18_64_                  =    "B16004_035",  # Speaks Indo-European language, English well, ages 18 to 64
        
         asian_well_18_64_                     =    "B16004_038",  # Speaks Asian/Pacific Islander language, English well, ages 18 to 64
         asian_notwell_18_64_                  =    "B16004_039",  # Speaks Asian/Pacific Islander language, English not well, ages 18 to 64
         asian_notatall_18_64_                 =    "B16004_040",  # Speaks Asian/Pacific Islander language, English not at all, ages 18 to 64
       
         other_well_18_64_                     =    "B16004_043",  # Speaks other language, English well, ages 18 to 64
         other_notwell_18_64_                  =    "B16004_044",  # Speaks other language, English well, ages 18 to 64
         other_notatall_18_64_                 =    "B16004_045",  # Speaks other language, English well, ages 18 to 64

# LEP, ages 65-plus

         spanish_well_65p_                     =    "B16004_050",  # Speaks Spanish, English well, ages 65 plus
         spanish_notwell_65p_                  =    "B16004_051",  # Speaks Spanish, English not well, ages 65 plus
         spanish_notatall_65p_                 =    "B16004_052",  # Speaks Spanish, English not at all, ages 65 plus
         
         indo_well_65p_                        =    "B16004_055",  # Speaks Indo-European language, English well, ages 65 plus
         indo_notwell_65p_                     =    "B16004_056",  # Speaks Indo-European language, English well, ages 65 plus
         indo_notatall_65p_                    =    "B16004_057",  # Speaks Indo-European language, English well, ages 65 plus
         
         asian_well_65p_                       =    "B16004_060",  # Speaks Asian/Pacific Islander language, English well, ages 65 plus
         asian_notwell_65p_                    =    "B16004_061",  # Speaks Asian/Pacific Islander language, English not well, ages 65 plus
         asian_notatall_65p_                   =    "B16004_062",  # Speaks Asian/Pacific Islander language, English not at all, ages 65 plus
         
         other_well_65p_                       =    "B16004_065",  # Speaks other language, English well, ages 65 plus
         other_notwell_65p_                    =    "B16004_066",  # Speaks other language, English well, ages 65 plus
         other_notatall_65p_                   =    "B16004_067")  # Speaks other language, English well, ages 65 plus


# Race/Ethnicity - ACS

race_acs <- c(race_total_                      =    "B03002_001",  # Total population
          race_white_                          =    "B03002_003",  # White population
          race_black_                          =    "B03002_004",  # Black population
          race_aian_                           =    "B03002_005",  # American Indian/Alaska Native population
          race_asian_                          =    "B03002_006",  # Asian population
          race_nhpi_                           =    "B03002_007",  # Native Hawaiian/Pacific Islander population
          race_other_                          =    "B03002_008",  # Other population
          race_twoplus_                        =    "B03002_009",  # Two-plus races
          race_hispanic_                       =    "B03002_012")  # Hispanic/Latino population

race_decennial <- c(race_total_E               =    "P008001",  # Total population    # Added "_E" for easier joining with ACS data, though not an "estimate"
              race_white_E                     =    "P008003",  # White population
              race_black_E                     =    "P008004",  # Black population
              race_aian_E                      =    "P008005",  # American Indian/Alaska Native population
              race_asian_E                     =    "P008006",  # Asian population
              race_nhpi_E                      =    "P008007",  # Native Hawaiian/Pacific Islander population
              race_other_E                     =    "P008008",  # Other population
              race_twoplus_E                   =    "P008009",  # Black population
              race_hispanic_E                  =    "P008010")  # Hispanic/Latino population

# Persons in poverty (less than 200 percent)

poverty_persons <- c(poverty_universe_         =    "C17002_001", # Total poverty universe (population for whom poverty status is determined)
                     poverty_2.00p_            =    "C17002_008") # Persons 200+ percent income to poverty ratio

# Older adults

older_adult     <- c(age_total_                =    "B01001_001", # Total population of any age or sex
                     male_75_79_               =    "B01001_023", # Male ages 75 to 79
                     male_80_84_               =    "B01001_024", # Male ages 75 to 79
                     male_85p_                 =    "B01001_025", # Male ages 75 to 79
                     female_75_79_             =    "B01001_047", # Female ages 75 to 79
                     female_80_84_             =    "B01001_048", # Female ages 75 to 79
                     female_85p_               =    "B01001_049") # Female ages 75 to 79

# Collapsed disability categories. Variable below is num_disabilities, age category

disability_collapsed  <- c(tot_dis_universe_   =    "C18108_001",
                           tot_under_18_       =    "C18108_002",
                           one_under_18_       =    "C18108_003",
                           twop_under_18_      =    "C18108_004",
                           nondis_under_18_    =    "C18108_005",
                           tot_18_64_          =    "C18108_006",
                           one_18_64_          =    "C18108_007",
                           twop_18_64_         =    "C18108_008", 
                           nondis_18_64_       =    "C18108_009",                  
                           tot_65p_            =    "C18108_010",
                           one_65p_            =    "C18108_011",
                           twop_65p_           =    "C18108_012",
                           nondis_65p_         =    "C18108_013")            
                                               
# Compile all the variables for use with county table (used for most of analyses)

total_acs_variables <- c(rent_burden,low_income_families,med_dis_earnings,disability,tenure,vehicles,lep,non_lep,
                         race_acs,med_inc_race,hholder_race)

# Create functions for extracting data, including ACS and decennial data functions by county, place, and tract
# Create ACS function for place data and join place type (big three cities, bayside cities, and inland/coastal/delta cities)
# Create decennial function for place data and join place type (big three cities, bayside cities, and inland/coastal/delta cities)
# In each case, remove MOE variables ("_M")

get_acs_county <- function(acs_year,acs_product,variables){
  get_acs(geography = "county",
          variables = variables,
          state = statenumber,
          county = baycounties,
          year = acs_year,
          survey = acs_product,
          output = "wide") %>% 
  select(-c(ends_with("_M"),GEOID)) %>% 
  mutate(year=acs_year)
}

get_decennial_county <- function(year,variables){
  get_decennial(geography = "county",
          variables = variables,
          state = statenumber,
          county = baycounties,
          year = year,
          sumfile = "sf1",
          output = "wide") %>% 
    select(-GEOID) %>% 
    mutate(year=year)
}

get_historical_place_acs <- function(year,variables) {
  get_acs(
    geography = "place", 
    variables = variables, 
    year = year, 
    state = statenumber,
    survey = "acs5",
    output = "wide"
  ) %>%
    select(-c(ends_with("_M"))) %>% 
    filter(GEOID %in% baycities) %>% 
    mutate(
      geography=case_when(
        GEOID %in% big_three      ~ "Big Three Cities",
        GEOID %in% bayside        ~ "Bayside Cities",
        GEOID %in% in_coast_delta ~ "Inland_Coastal_Delta Cities",
        TRUE                      ~ "Mistaken Coding"
      ),
      year=year
    ) 
}

get_historical_place_decennial <- function(year,variables){
  get_decennial(
    geography = "place",
    variables = variables,
    year = year,
    sumfile = "sf1",
    state = statenumber,
    output = "wide"
  ) %>%
    filter(GEOID %in% baycities) %>% 
    mutate(
      geography=case_when(
        GEOID %in% big_three      ~ "Big Three Cities",
        GEOID %in% bayside        ~ "Bayside Cities",
        GEOID %in% in_coast_delta ~ "Inland_Coastal_Delta Cities",
        TRUE                      ~ "Mistaken Coding"
      ),
      year=year
    ) 
}

get_historical_tract_bg_acs <- function(year,variables,tract_bg) {
  get_acs(
    geography = tract_bg, 
    variables = variables, 
    year = year, 
    state = statenumber,
    county=baycounties,
    survey = "acs5",
    output = "wide"
  ) %>%
    select(-c(ends_with("_M"))) %>% 
    mutate(year=year
    ) 
}



# Create working_county dataframe for most of the outputs

acs_year=2018
working_county <- get_acs_county(acs_year,"acs5",total_acs_variables)

# Summarize to Bay Area and begin individual analyses for outputs
# Multiply median disabled/non-disabled earnings by disabled/non-disabled workers, divide by total workers to get weighted mean earnings for Bay Area
# After doing a regional summation, do the division to get weighted regional median values

working_bay     <- working_county %>% 
  mutate(med_dis_earnings_x_dis_worker=med_dis_earnings_E*dis_worker_E,
         med_non_dis_earning_x_non_dis_worker=med_non_dis_earnings_E*non_dis_worker_E) %>% 
  select(-c(med_dis_earnings_E,med_non_dis_earnings_E,med_inc_white_E,med_inc_black_E,med_inc_asian_E,med_inc_hispanic_E)) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum)) %>%        
  mutate(geography = paste0("Bay_Area_",acs_year)) %>%             
  relocate(geography, .before = everything()) %>% 
  mutate(weighted_med_dis_earnings=round(med_dis_earnings_x_dis_worker/dis_worker_E),
         weighted_med_non_dis_earnings=round(med_non_dis_earning_x_non_dis_worker/non_dis_worker_E))

# Share rent burden 

rent_burden_df <- working_bay %>% 
  transmute(geography,
         share_rent_30_39=round(100*(rent_30_35_E + rent_35_40_E)/tot_rent_E),
         share_rent_40_49=round(100*(rent_40_50_E)/tot_rent_E),
         share_rent_50plus=round(100*(rent_50p_E)/tot_rent_E))


# Pie chart of low income by family type

pie_family <- working_bay %>% 
  transmute(geography,
            male_single_parent=male_under_1.30_E+male_1.30_1.49_E+male_1.50_1.84_E,
            female_single_parent=female_under_1.30_E+female_1.30_1.49_E+female_1.50_1.84_E,
            two_parents=married_under_1.30_E+married_1.30_1.49_E+married_1.50_1.84_E,
            total=male_single_parent+female_single_parent+two_parents,
            share_male_single_parent=round(100*(male_single_parent)/total),
            share_female_single_parent=round(100*(female_single_parent)/total),
            share_two_parents=round(100*(two_parents)/total))

# Share of families with low incomes

share_family <- working_bay %>% 
  transmute(geography,
            two_parent_under=married_under_1.30_E+married_1.30_1.49_E+married_1.50_1.84_E,
            male_parent_under=male_under_1.30_E+male_1.30_1.49_E+male_1.50_1.84_E,
            female_parent_under=female_under_1.30_E+female_1.30_1.49_E+female_1.50_1.84_E,
            single_parent_under=male_parent_under+female_parent_under,
            family_under=two_parent_under+single_parent_under,
            two_parent_total=two_parent_under+married_1.85p_E,
            male_parent_total=male_parent_under+male_1.85p_E,
            female_parent_total=female_parent_under+female_1.85p_E,
            single_parent_total=male_parent_total+female_parent_total,
            family_total=family_under+married_1.85p_E+male_1.85p_E+female_1.85p_E,
            share_regionwide_family_under=round(100*(family_under/family_total)),            
            share_twoparent_under=round(100*(two_parent_under/two_parent_total)),
            share_singleparent_under=round(100*(single_parent_under/single_parent_total)),
            share_female_head_under=round(100*(female_parent_under/female_parent_total)),
            share_male_head_under=round(100*(male_parent_under/male_parent_total)))

# Median earnings by disability status, ACS 1-year data 2012-2022, omitting 2020 not available due to Covid
# Calculate weighted median for Bay Area for disabled workers and non-disabled workers

med_disability_earnings <- map_dfr(c(2012:2019,2021:2022),~ get_acs_county (.x,"acs1",med_dis_earnings)) %>% # 2020 data not available due to pandemic
  mutate(med_dis_earnings_x_dis_worker=med_dis_earnings_E*dis_worker_E,
         med_non_dis_earning_x_non_dis_worker=med_non_dis_earnings_E*non_dis_worker_E) %>% 
  group_by(year) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum),.groups = "drop") %>%   
  select(-c(med_dis_earnings_E,med_non_dis_earnings_E)) %>% 
  mutate(geography = "Bay_Area") %>%             
  relocate(geography, .before = everything()) %>% 
  mutate(weighted_med_non_dis_earnings=round(med_non_dis_earning_x_non_dis_worker/non_dis_worker_E),
         weighted_med_dis_earnings=round(med_dis_earnings_x_dis_worker/dis_worker_E),
         ) 


# Share of population with disabilities
# Some totals are built from component parts and some totals already exist in the data for some groups (e.g., white_dis_universe)
# "Other" is the sum of American Indian/Alaska Native, Native Hawaiian/PI, other, and two-plus races

share_disabled <- working_bay %>% 
  transmute(geography,
            under18_disabled=male_dis_under5_E+male_dis_5_17_E+female_dis_under5_E+female_dis_5_17_E,
            age18_64_disabled=male_dis_18_34_E+male_dis_35_64_E+female_dis_18_34_E+female_dis_35_64_E,
            age65p_disabled=male_dis_65_74_E+male_dis_75p_E+female_dis_65_74_E+female_dis_75p_E,
            under18_total=under18_disabled+male_non_dis_under5_E+male_non_dis_5_17_E+female_non_dis_under5_E+female_non_dis_5_17_E,
            age18_64_total=age18_64_disabled+male_non_dis_18_34_E+male_non_dis_35_64_E+female_non_dis_18_34_E+female_non_dis_35_64_E,
            age65p_total=age65p_disabled+male_non_dis_65_74_E+male_non_dis_75p_E+female_non_dis_65_74_E+female_non_dis_75p_E,
            black_disabled=black_dis_under18_E+black_dis_18_64_E+black_dis_65p_E,
            hispanic_disabled=hispanic_dis_under18_E+hispanic_dis_18_64_E+hispanic_dis_65p_E,
            asian_disabled=asian_dis_under18_E+asian_dis_18_64_E+asian_dis_65p_E,
            white_disabled=white_dis_under18_E+white_dis_18_64_E+white_dis_65p_E,
            other_disabled=aian_dis_under18_E+aian_dis_18_64_E+aian_dis_65p_E+nhpi_dis_under18_E+nhpi_dis_18_64_E+nhpi_dis_65p_E+
              other_dis_under18_E+other_dis_18_64_E+other_dis_65p_E+twoplus_dis_under18_E+twoplus_dis_18_64_E+twoplus_dis_65p_E,
            other_total=aian_dis_universe_E+nhpi_dis_universe_E+other_dis_universe_E+twoplus_dis_universe_E,
            share_regionwide_disabled=round(100*(under18_disabled+age18_64_disabled+age65p_disabled)/tot_dis_universe_E),
            share_under18_disabled=round(100*under18_disabled/under18_total),
            share18_64_disabled=round(100*age18_64_disabled/age18_64_total),
            share65p_disabled=round(100*age65p_disabled/age65p_total),
            share_black_disabled=round(100*black_disabled/black_dis_universe_E),
            share_hispanic_disabled=round(100*hispanic_disabled/hispanic_dis_universe_E),
            share_asian_disabled=round(100*asian_disabled/asian_dis_universe_E),
            share_white_disabled=round(100*white_disabled/white_dis_universe_E),
            share_other_disabled=round(100*other_disabled/other_total))
         
         
# Senior tenure by place type
# Get regional data from sum of counties (Bay Area file)
# For city data, call it from an ACS place call, join with equivalencies for place type, summarize

senior_tenure_bay <- working_bay %>% 
  transmute(geography,
            share_senior_renter=round(100*(renter_75_84_E+renter_85p_E)/(renter_75_84_E+renter_85p_E+owner_75_84_E+owner_85p_E)))

acs_year=2022
senior_tenure_place <- get_historical_place_acs(acs_year,tenure) %>% 
  group_by(geography) %>% 
  summarize(owner_75_84_E=sum(owner_75_84_E),owner_85p_E=sum(owner_85p_E),renter_75_84_E=sum(renter_75_84_E),
            renter_85p_E=sum(renter_85p_E),.groups = "drop") %>% 
  transmute(geography=paste0(geography,"_",acs_year),
            share_senior_renter=round(100*(renter_75_84_E+renter_85p_E)/(renter_75_84_E+renter_85p_E+owner_75_84_E+owner_85p_E)))

senior_tenure <- bind_rows(senior_tenure_bay,senior_tenure_place)

# Zero-vehicle households

zero_vehicles <- working_bay %>% 
  transmute(geography,
            share_zero_veh_renters=round(100*(zero_renter_all_E/all_renter_all_E)),
            share_zero_veh_homeowners=round(100*(zero_owner_all_E/all_owner_all_E)),
            share_zero_veh_15_34=round(100*((zero_owner_15_34_E+zero_renter_15_34_E)/(zero_owner_15_34_E+zero_renter_15_34_E+onep_owner_15_34_E+onep_renter_15_34_E))),
            share_zero_veh_35_64=round(100*((zero_owner_35_64_E+zero_renter_35_64_E)/(zero_owner_35_64_E+zero_renter_35_64_E+onep_owner_35_64_E+onep_renter_35_64_E))),
            share_zero_veh_65p=round(100*((zero_owner_65p_E+zero_renter_65p_E)/(zero_owner_65p_E+zero_renter_65p_E+onep_owner_65p_E+onep_renter_65p_E)))                           
  )

# English proficiency

english_proficiency <- working_bay %>% 
  transmute(geography,
            lep_5_17=spanish_notwell_5_17_E+spanish_notatall_5_17_E+indo_notwell_5_17_E+indo_notatall_5_17_E+
              asian_notwell_5_17_E+asian_notatall_5_17_E+other_notwell_5_17_E+other_notatall_5_17_E,
            
            lep_18_64=spanish_notwell_18_64_E+spanish_notatall_18_64_E+indo_notwell_18_64_E+indo_notatall_18_64_E+
              asian_notwell_18_64_E+asian_notatall_18_64_E+other_notwell_18_64_E+other_notatall_18_64_E,
            
            lep_65p=spanish_notwell_65p_E+spanish_notatall_65p_E+indo_notwell_65p_E+indo_notatall_65p_E+
              asian_notwell_65p_E+asian_notatall_65p_E+other_notwell_65p_E+other_notatall_65p_E,
            
            non_lep_5_17=english_only_5_17_E+spanish_vwell_5_17_E+indo_vwell_5_17_E+asian_vwell_5_17_E+other_vwell_5_17_E+
              spanish_well_5_17_E+indo_well_5_17_E+asian_well_5_17_E+other_well_5_17_E,
            
            non_lep_18_64=english_only_18_64_E+spanish_vwell_18_64_E+indo_vwell_18_64_E+asian_vwell_18_64_E+other_vwell_18_64_E+
              spanish_well_18_64_E+indo_well_18_64_E+asian_well_18_64_E+other_well_18_64_E,
            
            non_lep_65p=english_only_65p_E+spanish_vwell_65p_E+indo_vwell_65p_E+asian_vwell_65p_E+other_vwell_65p_E+
              spanish_well_65p_E+indo_well_65p_E+asian_well_65p_E+other_well_65p_E,
            
            share5_17_lep=round(100*(lep_5_17/(lep_5_17+non_lep_5_17))),
            share18_64_lep=round(100*(lep_18_64/(lep_18_64+non_lep_18_64))),
            share65p_lep=round(100*(lep_65p/(lep_65p+non_lep_65p)))
            )

# Race/ethnicity by place type, first ACS for 2009-2022 and then decennial for 2000
# Combine datasets, calculate racial category shares within each geography type (sum to 100 percent for all Bay Area places/cities)
# Order data frame by year, geography type

historical_race_acs_place <- map_dfr(c(2009,2014,2018,2022),~ get_historical_place_acs(.x, race_acs)) 
historical_race_decennial_place <- get_historical_place_decennial(2000,race_decennial) 
historical_race_composite_place <- bind_rows(historical_race_decennial_place,historical_race_acs_place) %>% arrange(geography,year)

historical_race_bay_place <- historical_race_composite_place %>% 
  group_by(year) %>% 
  summarize(geography="Sum of All Bay Places (Cities and Towns)",race_total=sum(race_total_E),race_white=sum(race_white_E),race_black=sum(race_black_E),race_asian=sum(race_asian_E),
            race_hispanic=sum(race_hispanic_E),.groups = "drop")

historical_race_grouped_places <- historical_race_composite_place %>% 
  group_by(year,geography) %>% 
  summarize(race_total=sum(race_total_E),race_white=sum(race_white_E),race_black=sum(race_black_E),race_asian=sum(race_asian_E),
            race_hispanic=sum(race_hispanic_E),.groups = "drop")

custom_order <- c("Big Three Cities", "Bayside Cities", 
                  "Inland_Coastal_Delta Cities", "Sum of All Bay Places (Cities and Towns)") # For sorting below data frame


historical_race_final <- bind_rows(historical_race_grouped_places,historical_race_bay_place) %>% 
  group_by(year) %>% 
  mutate(share_white=round(100*race_white/race_white[geography=="Sum of All Bay Places (Cities and Towns)"]),
         share_asian=round(100*race_asian/race_asian[geography=="Sum of All Bay Places (Cities and Towns)"]),
         share_hispanic=round(100*race_hispanic/race_hispanic[geography=="Sum of All Bay Places (Cities and Towns)"]),
         share_black=round(100*race_black/race_black[geography=="Sum of All Bay Places (Cities and Towns)"])
         ) %>% 
  mutate(geography = factor(geography, levels = custom_order)) %>%
  arrange(year,geography)

# Median household income by race of householder
# Some counties don't have enough households of particular groups for representative data and NAs are in the dataset - changed NA values to 0
# This was particularly an issue with Napa County. Using 5-year ACS data would likely solve the problem. 

acs_year <- 2018
race_income_vars <- c(med_inc_race,hholder_race)

med_household_income <- get_acs_county(acs_year, "acs1",race_income_vars) %>% 
  mutate(med_inc_asian_x_hholder_asian = med_inc_asian_E * hholder_asian_E,
         med_inc_white_x_hholder_white = med_inc_white_E * hholder_white_E,
         med_inc_hispanic_x_hholder_hispanic = med_inc_hispanic_E * hholder_hispanic_E,
         med_inc_black_x_hholder_black = med_inc_black_E * hholder_black_E
  ) %>% 
  select(-c(med_inc_asian_E, med_inc_white_E, med_inc_hispanic_E, med_inc_black_E)) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%  # Replace NAs with 0
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  transmute(geography = paste0("Bay_Area_", acs_year),
            weighted_med_inc_asian = round(med_inc_asian_x_hholder_asian / hholder_asian_E,-2),
            weighted_med_inc_white = round(med_inc_white_x_hholder_white / hholder_white_E,-2),
            weighted_med_inc_hispanic = round(med_inc_hispanic_x_hholder_hispanic / hholder_hispanic_E,-2),
            weighted_med_inc_black = round(med_inc_black_x_hholder_black / hholder_black_E,-2)) %>%
  relocate(geography, .before = everything())

# Share of population by race, Census 2000 and ACS 2014/2018 5-year

historical_race_acs_county <- map_dfr(c(2014,2018,2022),~ get_acs_county(.x, "acs5",race_acs))
historical_race_decennial_county <- get_decennial_county(2000,race_decennial)

historical_race_composite_county <- bind_rows(historical_race_decennial_county,historical_race_acs_county) %>% 
  group_by(year) %>% 
  mutate(race_other=race_aian_E+race_nhpi_E+race_other_E+race_twoplus_E) %>% 
  summarize(geography="Bay Area",race_other=sum(race_other),race_black=sum(race_black_E),race_asian=sum(race_asian_E),
            race_hispanic=sum(race_hispanic_E),race_white=sum(race_white_E),race_total=sum(race_total_E)) %>% 
  transmute(geography=paste(geography,"_",year),share_other=round(100*race_other/race_total),share_black=round(100*race_black/race_total),share_asian=round(100*race_asian/race_total),
            share_hispanic=round(100*race_hispanic/race_total),share_white=round(100*race_white/race_total),race_total)

# Extract tract data for 2018 and 2022 and join EPCs/HRAs with respective years
# Extract block group data for 2018 HRAs, join with HRA tract data
# Calculate share of population by demographic, 2022

tract_vars <- c(race_acs,poverty_persons,lep,non_lep,vehicles,older_adult,disability_collapsed,families,rent_burden)
bg_vars <- c(race_acs,poverty_persons,lep,non_lep,vehicles,older_adult,families,rent_burden)

region_collapsed <- get_acs_county(2018,"acs5",tract_vars) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  mutate(geography="Bay Area_2018") %>% 
  relocate(geography,.before = everything()) %>% 
  mutate(
    lep_5_17=spanish_notwell_5_17_E+spanish_notatall_5_17_E+indo_notwell_5_17_E+indo_notatall_5_17_E+
      asian_notwell_5_17_E+asian_notatall_5_17_E+other_notwell_5_17_E+other_notatall_5_17_E,
    
    lep_18_64=spanish_notwell_18_64_E+spanish_notatall_18_64_E+indo_notwell_18_64_E+indo_notatall_18_64_E+
      asian_notwell_18_64_E+asian_notatall_18_64_E+other_notwell_18_64_E+other_notatall_18_64_E,
    
    lep_65p=spanish_notwell_65p_E+spanish_notatall_65p_E+indo_notwell_65p_E+indo_notatall_65p_E+
      asian_notwell_65p_E+asian_notatall_65p_E+other_notwell_65p_E+other_notatall_65p_E,
    
    non_lep_5_17=english_only_5_17_E+spanish_vwell_5_17_E+indo_vwell_5_17_E+asian_vwell_5_17_E+other_vwell_5_17_E+
      spanish_well_5_17_E+indo_well_5_17_E+asian_well_5_17_E+other_well_5_17_E,
    
    non_lep_18_64=english_only_18_64_E+spanish_vwell_18_64_E+indo_vwell_18_64_E+asian_vwell_18_64_E+other_vwell_18_64_E+
      spanish_well_18_64_E+indo_well_18_64_E+asian_well_18_64_E+other_well_18_64_E,
    
    non_lep_65p=english_only_65p_E+spanish_vwell_65p_E+indo_vwell_65p_E+asian_vwell_65p_E+other_vwell_65p_E+
      spanish_well_65p_E+indo_well_65p_E+asian_well_65p_E+other_well_65p_E,
    
    lep_total=lep_5_17+lep_18_64+lep_65p,
    non_lep_total=non_lep_5_17+non_lep_18_64+non_lep_65p,
    lep_universe=lep_total+non_lep_total
  )

historical_tracts_2018 <- get_historical_tract_bg_acs(2018,tract_vars,"tract") %>% left_join(.,epc_2018 %>% select(-County.FIPS),by=c("GEOID"="Geographic.ID")) %>% 
  mutate(PBA.2050.Equity.Priority.Community=if_else(is.na(PBA.2050.Equity.Priority.Community),0,PBA.2050.Equity.Priority.Community)) %>% 
  relocate(PBA.2050.Equity.Priority.Community,.after = "NAME") %>% 
  left_join(.,hra_tracts %>% select(-c(fipco,blkgp_geoi)),by=c("GEOID"="tract_geoi")) %>% 
  mutate(hra_status=if_else(is.na(oppcat),0,1)) %>% 
  relocate(c(hra_status,oppcat),.after = "NAME")

historical_bgs_2018 <- get_historical_tract_bg_acs(2018,bg_vars,"block group") %>%  
  left_join(.,hra_bgs %>% select(-c(fipco,tract_geoi)),by=c("GEOID"="blkgp_geoi")) %>% 
  mutate(hra_status=if_else(is.na(oppcat),0,1)) %>% 
  relocate(c(hra_status,oppcat),.after = "NAME")


historical_tracts_2022 <- get_historical_tract_bg_acs(2022,tract_vars,"tract") %>% left_join(.,epc_2022 %>% select(-County.FIPS),by=c("GEOID"="Geographic.ID")) %>% 
  mutate(Equity.Priority.Community.PBA.2050.Plus=if_else(is.na(Equity.Priority.Community.PBA.2050.Plus),0,Equity.Priority.Community.PBA.2050.Plus)) %>% 
  relocate(Equity.Priority.Community.PBA.2050.Plus,.after = "NAME") 

share_population_bay_area <- region_collapsed %>% 
  transmute(geography,
            share_poc=round(100*((race_total_E-race_white_E)/race_total_E)),
            share_low_income=round(100*((poverty_universe_E-poverty_2.00p_E)/poverty_universe_E)),
            share_lep=round(100*(lep_total/lep_universe)),
            share_zero_veh=round(100*((zero_renter_all_E+zero_owner_all_E)/(all_owner_all_E+all_renter_all_E))),
            share_older_adult=round(100*((male_75_79_E+male_80_84_E+male_85p_E+female_75_79_E+female_80_84_E+female_85p_E)/age_total_E)),
            share_disabled=round(100*((one_under_18_E+twop_under_18_E+one_18_64_E+twop_18_64_E+one_65p_E+twop_65p_E)/tot_dis_universe_E)),
            share_single_parent=round(100*((male_householder_E+female_householder_E)/(male_householder_E+female_householder_E+married_family_E))),
            share_rent_burdened=round(100*(rent_50p_E/tot_rent_E))
            )

share_epc_2018 <- historical_tracts_2018 %>% 
  filter(PBA.2050.Equity.Priority.Community==1) %>% 
  select(-c(GEOID,hra_status,oppcat,PBA.2050.Equity.Priority.Community,year)) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  mutate(geography="EPC_2018") %>% 
  relocate(geography,.before = everything()) %>% 
  mutate(
    lep_5_17=spanish_notwell_5_17_E+spanish_notatall_5_17_E+indo_notwell_5_17_E+indo_notatall_5_17_E+
      asian_notwell_5_17_E+asian_notatall_5_17_E+other_notwell_5_17_E+other_notatall_5_17_E,
    
    lep_18_64=spanish_notwell_18_64_E+spanish_notatall_18_64_E+indo_notwell_18_64_E+indo_notatall_18_64_E+
      asian_notwell_18_64_E+asian_notatall_18_64_E+other_notwell_18_64_E+other_notatall_18_64_E,
    
    lep_65p=spanish_notwell_65p_E+spanish_notatall_65p_E+indo_notwell_65p_E+indo_notatall_65p_E+
      asian_notwell_65p_E+asian_notatall_65p_E+other_notwell_65p_E+other_notatall_65p_E,
    
    non_lep_5_17=english_only_5_17_E+spanish_vwell_5_17_E+indo_vwell_5_17_E+asian_vwell_5_17_E+other_vwell_5_17_E+
      spanish_well_5_17_E+indo_well_5_17_E+asian_well_5_17_E+other_well_5_17_E,
    
    non_lep_18_64=english_only_18_64_E+spanish_vwell_18_64_E+indo_vwell_18_64_E+asian_vwell_18_64_E+other_vwell_18_64_E+
      spanish_well_18_64_E+indo_well_18_64_E+asian_well_18_64_E+other_well_18_64_E,
    
    non_lep_65p=english_only_65p_E+spanish_vwell_65p_E+indo_vwell_65p_E+asian_vwell_65p_E+other_vwell_65p_E+
      spanish_well_65p_E+indo_well_65p_E+asian_well_65p_E+other_well_65p_E,
    
    lep_total=lep_5_17+lep_18_64+lep_65p,
    non_lep_total=non_lep_5_17+non_lep_18_64+non_lep_65p,
    lep_universe=lep_total+non_lep_total
  ) %>% 
  transmute(geography,
            share_poc=round(100*((race_total_E-race_white_E)/race_total_E)),
            share_low_income=round(100*((poverty_universe_E-poverty_2.00p_E)/poverty_universe_E)),
            share_lep=round(100*(lep_total/lep_universe)),
            share_zero_veh=round(100*((zero_renter_all_E+zero_owner_all_E)/(all_owner_all_E+all_renter_all_E))),
            share_older_adult=round(100*((male_75_79_E+male_80_84_E+male_85p_E+female_75_79_E+female_80_84_E+female_85p_E)/age_total_E)),
            share_disabled=round(100*((one_under_18_E+twop_under_18_E+one_18_64_E+twop_18_64_E+one_65p_E+twop_65p_E)/tot_dis_universe_E)),
            share_single_parent=round(100*((male_householder_E+female_householder_E)/(male_householder_E+female_householder_E+married_family_E))),
            share_rent_burdened=round(100*(rent_50p_E/tot_rent_E))
  )

share_epc_2022 <- historical_tracts_2022 %>% 
  filter(Equity.Priority.Community.PBA.2050.Plus==1) %>% 
  select(-c(GEOID,Equity.Priority.Community.PBA.2050.Plus,year)) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  mutate(geography="EPC_2022") %>% 
  relocate(geography,.before = everything()) %>% 
  mutate(
    lep_5_17=spanish_notwell_5_17_E+spanish_notatall_5_17_E+indo_notwell_5_17_E+indo_notatall_5_17_E+
      asian_notwell_5_17_E+asian_notatall_5_17_E+other_notwell_5_17_E+other_notatall_5_17_E,
    
    lep_18_64=spanish_notwell_18_64_E+spanish_notatall_18_64_E+indo_notwell_18_64_E+indo_notatall_18_64_E+
      asian_notwell_18_64_E+asian_notatall_18_64_E+other_notwell_18_64_E+other_notatall_18_64_E,
    
    lep_65p=spanish_notwell_65p_E+spanish_notatall_65p_E+indo_notwell_65p_E+indo_notatall_65p_E+
      asian_notwell_65p_E+asian_notatall_65p_E+other_notwell_65p_E+other_notatall_65p_E,
    
    non_lep_5_17=english_only_5_17_E+spanish_vwell_5_17_E+indo_vwell_5_17_E+asian_vwell_5_17_E+other_vwell_5_17_E+
      spanish_well_5_17_E+indo_well_5_17_E+asian_well_5_17_E+other_well_5_17_E,
    
    non_lep_18_64=english_only_18_64_E+spanish_vwell_18_64_E+indo_vwell_18_64_E+asian_vwell_18_64_E+other_vwell_18_64_E+
      spanish_well_18_64_E+indo_well_18_64_E+asian_well_18_64_E+other_well_18_64_E,
    
    non_lep_65p=english_only_65p_E+spanish_vwell_65p_E+indo_vwell_65p_E+asian_vwell_65p_E+other_vwell_65p_E+
      spanish_well_65p_E+indo_well_65p_E+asian_well_65p_E+other_well_65p_E,
    
    lep_total=lep_5_17+lep_18_64+lep_65p,
    non_lep_total=non_lep_5_17+non_lep_18_64+non_lep_65p,
    lep_universe=lep_total+non_lep_total
  ) %>% 
  transmute(geography,
            share_poc=round(100*((race_total_E-race_white_E)/race_total_E)),
            share_low_income=round(100*((poverty_universe_E-poverty_2.00p_E)/poverty_universe_E)),
            share_lep=round(100*(lep_total/lep_universe)),
            share_zero_veh=round(100*((zero_renter_all_E+zero_owner_all_E)/(all_owner_all_E+all_renter_all_E))),
            share_older_adult=round(100*((male_75_79_E+male_80_84_E+male_85p_E+female_75_79_E+female_80_84_E+female_85p_E)/age_total_E)),
            share_disabled=round(100*((one_under_18_E+twop_under_18_E+one_18_64_E+twop_18_64_E+one_65p_E+twop_65p_E)/tot_dis_universe_E)),
            share_single_parent=round(100*((male_householder_E+female_householder_E)/(male_householder_E+female_householder_E+married_family_E))),
            share_rent_burdened=round(100*(rent_50p_E/tot_rent_E))
  )

# High resource areas (other than disability, which didn't have block group data and has a different process below)

hra_2018_tracts_data <- historical_tracts_2018 %>% 
  filter(hra_status==1)

hra_2018_bgs_data <- historical_bgs_2018 %>% 
  filter(hra_status==1)

share_hra_non_disability_data <- bind_rows(hra_2018_tracts_data,hra_2018_bgs_data) %>% 
  select(-c(GEOID,hra_status,oppcat,PBA.2050.Equity.Priority.Community,year)) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  mutate(geography="HRA_2018") %>% 
  relocate(geography,.before = everything()) %>% 
  mutate(
    lep_5_17=spanish_notwell_5_17_E+spanish_notatall_5_17_E+indo_notwell_5_17_E+indo_notatall_5_17_E+
      asian_notwell_5_17_E+asian_notatall_5_17_E+other_notwell_5_17_E+other_notatall_5_17_E,
    
    lep_18_64=spanish_notwell_18_64_E+spanish_notatall_18_64_E+indo_notwell_18_64_E+indo_notatall_18_64_E+
      asian_notwell_18_64_E+asian_notatall_18_64_E+other_notwell_18_64_E+other_notatall_18_64_E,
    
    lep_65p=spanish_notwell_65p_E+spanish_notatall_65p_E+indo_notwell_65p_E+indo_notatall_65p_E+
      asian_notwell_65p_E+asian_notatall_65p_E+other_notwell_65p_E+other_notatall_65p_E,
    
    non_lep_5_17=english_only_5_17_E+spanish_vwell_5_17_E+indo_vwell_5_17_E+asian_vwell_5_17_E+other_vwell_5_17_E+
      spanish_well_5_17_E+indo_well_5_17_E+asian_well_5_17_E+other_well_5_17_E,
    
    non_lep_18_64=english_only_18_64_E+spanish_vwell_18_64_E+indo_vwell_18_64_E+asian_vwell_18_64_E+other_vwell_18_64_E+
      spanish_well_18_64_E+indo_well_18_64_E+asian_well_18_64_E+other_well_18_64_E,
    
    non_lep_65p=english_only_65p_E+spanish_vwell_65p_E+indo_vwell_65p_E+asian_vwell_65p_E+other_vwell_65p_E+
      spanish_well_65p_E+indo_well_65p_E+asian_well_65p_E+other_well_65p_E,
    
    lep_total=lep_5_17+lep_18_64+lep_65p,
    non_lep_total=non_lep_5_17+non_lep_18_64+non_lep_65p,
    lep_universe=lep_total+non_lep_total
  ) %>% 
  transmute(geography,
            share_poc=round(100*((race_total_E-race_white_E)/race_total_E)),
            share_low_income=round(100*((poverty_universe_E-poverty_2.00p_E)/poverty_universe_E)),
            share_lep=round(100*(lep_total/lep_universe)),
            share_zero_veh=round(100*((zero_renter_all_E+zero_owner_all_E)/(all_owner_all_E+all_renter_all_E))),
            share_older_adult=round(100*((male_75_79_E+male_80_84_E+male_85p_E+female_75_79_E+female_80_84_E+female_85p_E)/age_total_E)),
            share_single_parent=round(100*((male_householder_E+female_householder_E)/(male_householder_E+female_householder_E+married_family_E))),
            share_rent_burdened=round(100*(rent_50p_E/tot_rent_E))
  )

# Now work with disability data. Because we don't have block group level disability data, use the tract data that comprise the missing block groups
# Last digit is block group, so remove that

hra_bgs_as_tracts <- hra_bgs %>% 
  mutate(tract_geoi=substr(blkgp_geoi, 1, nchar(blkgp_geoi) - 1))

hra_tracts_and_bgs_as_tracts <- bind_rows(hra_tracts,hra_bgs_as_tracts) %>% 
  mutate(hra_status=1) %>% 
  select(tract_geoi,hra_status)

historical_tracts_2018_join <- historical_tracts_2018 %>% 
  select(GEOID,NAME,tot_dis_universe_E,tot_under_18_E,one_under_18_E,twop_under_18_E,nondis_under_18_E,
         tot_18_64_E,one_18_64_E,twop_18_64_E,nondis_18_64_E, tot_65p_E,one_65p_E,twop_65p_E,nondis_65p_E,year)

hra_disability_data <- left_join(hra_tracts_and_bgs_as_tracts,historical_tracts_2018_join, by=c("tract_geoi"="GEOID"))

share_hra_disability <- hra_disability_data %>% 
  select(-c(hra_status,year)) %>% 
  select(where(is.numeric)) %>%                   
  summarize(across(everything(), sum), .groups = "drop") %>%
  mutate(geography="HRA_2018") %>% 
  relocate(geography,.before = everything()) %>% 
  transmute(
    geography,
    share_disabled=round(100*((one_under_18_E+twop_under_18_E+one_18_64_E+twop_18_64_E+one_65p_E+twop_65p_E)/tot_dis_universe_E)),
)

share_hra_final_2018 <- left_join(share_hra_non_disability_data,share_hra_disability,by="geography")

## Export CSVs to appropriate project folders

write.csv(rent_burden_df,file.path(output,"1_rent_burden","rent_burden.csv"),row.names = F) 
write.csv(pie_family,file.path(output,"2_low_income_pie_chart","pie_low_income_families.csv"),row.names = F) 
write.csv(share_family,file.path(output,"3_low_income_families","low_income_families.csv"),row.names = F) 
write.csv(med_disability_earnings,file.path(output,"4_med_disability_earnings","med_disability_earnings.csv"),row.names = F)
write.csv(share_disabled,file.path(output,"5_share_disabled","share_disabled.csv"),row.names = F)
write.csv(senior_tenure,file.path(output,"6_share_senior_renter","share_senior_renter.csv"),row.names = F)
write.csv(zero_vehicles,file.path(output,"7_zero_vehicles","zero_vehicles.csv"),row.names = F)
write.csv(english_proficiency,file.path(output,"8_english_proficiency","english_proficiency.csv"),row.names = F)
write.csv(historical_race_final,file.path(output,"9_historical_race_place_type","historical_race_place_type.csv"),row.names = F)
write.csv(med_household_income,file.path(output,"10_med_household_income","med_household_income.csv"),row.names = F) 
write.csv(historical_race_composite_county,file.path(output,"11_share_race_historical","share_race_historical.csv"),row.names = F) 
write.csv(share_population_bay_area,file.path(output,"12_share_population_by_demographic","share_population_bay_area.csv"),row.names = F) 
write.csv(share_epc_2018,file.path(output,"12_share_population_by_demographic","share_epc_2018.csv"),row.names = F) 
write.csv(share_epc_2022,file.path(output,"12_share_population_by_demographic","share_epc_2022.csv"),row.names = F) 
write.csv(share_hra_final_2018,file.path(output,"12_share_population_by_demographic","share_hra_2018.csv"),row.names = F) 









