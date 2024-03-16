##############################################################################
#  Workshop_Script_1.r
#   R scripts used in the Workshop: "Using R for Census Data Analysis:
#       There's a Package for That"
#   Presented at the 2022 Census Data for Transportation Planning Conference
#     Reno, Nevada, June 8, 2022
#
#   Chuck Purvis scripts, only.
#
#   Re-presented at the Bay Area Model Coordination Working Group on 8/3/2022
#     edited 8/3/2022
###############################################################################

#  Section 1. Decennial Census
#
# Get relevant PL94-171 variables for ALL US counties, Census 2020

# Load the relevant R packages into the current R session
library(tidyverse)
library(tidycensus)

# load_variables creates a data frame with a list of tables and table cells.
varlist19 <- load_variables(2019,"acs1",cache=FALSE)

var_1990_pl  <- load_variables(1990,"pl",cache=FALSE) # Doesn't work. Endpoint is 2000.
var_2000_sf1 <- load_variables(2000,"sf1",cache=FALSE)
var_2000_pl  <- load_variables(2000,"pl",cache=FALSE)
var_2010_sf1 <- load_variables(2010,"sf1",cache=FALSE)
var_2010_pl  <- load_variables(2010,"pl",cache=FALSE)
var_2020_pl  <- load_variables(2020,"pl",cache=FALSE)

subj08 <- load_variables(2008,"acs1/subject",cache=TRUE) # doesn't work
subj09 <- load_variables(2009,"acs1/subject",cache=TRUE) # doesn't work
subj10 <- load_variables(2010,"acs1/subject",cache=TRUE)
subj19 <- load_variables(2019,"acs1/subject",cache=TRUE)
subj20 <- load_variables(2020,"acs5/subject",cache=TRUE)
dp20   <- load_variables(2020,"acs5/profile",cache=TRUE)

selvars20  <- c(TotalPop20 = "P2_001N",   # Total Population
                Hispanic20 = "P2_002N",   # Hispanic or Latino
                NH_White20 = "P2_005N",   # Non-Hispanic, White alone
                NH_Black20 = "P2_006N")   # Non-Hispanic, Black alone

us_counties2020 <- get_decennial(year=2020,  sumfile="pl", 
                                 geography = "county", # state="Nevada",
                                 #   geometry=TRUE, keep_geo_vars=TRUE,
                                 show_call = TRUE, output="wide", variables = selvars20) %>% 
                   dplyr::arrange(GEOID)

sortpop <- us_counties2020 %>% 
           dplyr::arrange(desc(TotalPop20)) %>% 
           dplyr::mutate(share_hispanic=Hispanic20/TotalPop20,
                         share_white   =NH_White20/TotalPop20,
                         share_black   =NH_Black20/TotalPop20)

density1 <- tidycensus::get_decennial(year=2020,  sumfile="pl", 
                                      geography = "county", # state="Nevada",
                                      geometry=TRUE, keep_geo_vars=TRUE,
                                      show_call = TRUE, output="wide", variables = selvars20) %>% 
            dplyr::mutate(landarea_sqmi20 = ALAND / 2589988.10, # convert from square meters to square miles
                          watrarea_sqmi20 = AWATER / 2589988.10,
                          pop_density20 = TotalPop20 / landarea_sqmi20) %>% 
            dplyr::arrange(desc(pop_density20)) %>% 
            dplyr::relocate(STATEFP:AWATER,.after=pop_density20) %>% 
            dplyr::relocate(pop_density20,.before=TotalPop20)

#  Section 2. American Community Survey
#

alphabeta1  <- get_acs(survey="acs1", year=2009, geography = "county",   state = "CA", 
                     county=c(1,13,41,55,75,81,85,95,97),
                     show_call = TRUE,output="wide",
                     variables = c(Total_09_     = "C27001_001",  # Total, Civlian NonInstit.
                                   M_With_LT18_09_ = "C27001_004",  # With Insurance, < 18 male
                                   M_NoIn_LT18_09_ = "C27001_005",  # Without Insurance, < 18 male
                                   M_With_1864_09_ = "C27001_007",  # With Insurance, 18-64 male
                                   M_NoIn_1864_09_ = "C27001_008",  # Without Insurance, 18-64 male
                                   M_With_6599_09_ = "C27001_010",  # With Insurance, 65+ male
                                   M_NoIn_6599_09_ = "C27001_011",  # Without Insurance, 65+ male
                                   F_With_LT18_09_ = "C27001_014",  # With Insurance, < 18 female
                                   F_NoIn_LT18_09_ = "C27001_015",  # Without Insurance, < 18 female
                                   F_With_1864_09_ = "C27001_017",  # With Insurance, 18-64 female
                                   F_NoIn_1864_09_ = "C27001_018",  # Without Insurance, 18-64 female
                                   F_With_6599_09_ = "C27001_020",  # With Insurance, 65+ female
                                   F_NoIn_6599_09_ = "C27001_021")) 

# 
acs20  <- load_variables(2020,"acs5",cache=TRUE)          # n=27,850 variables (B and C tables)
subj20 <- load_variables(2020,"acs5/subject",cache=TRUE)  # n=18,775 variables (S tables)
dp20   <- load_variables(2020,"acs5/profile",cache=TRUE)  # n=1,354 variable (DP tables)

selvars <- c(households_     = "B25044_001", # Total Households
             totalpop_       = "B01001_001", # Total Population (HHPOP+GQPOP)
             hhpop_total_    = "B25008_001", # Population in Households, Total
             
             households2_       = "DP02_0152",  # Total Households
             hh_with_computer_  = "DP02_0153",  # Households with Computers
             hh_with_broadband_ = "DP02_0154",  # Households with Broadband Internet
             
             totpop_civ_     = "S2701_C01_001", # Total Civilian Noninstitutional Population
             totpop_withins_ = "S2701_C02_001", # Total Civ Population, WITH Health Insurance
             pct_insured_    = "S2701_C03_001", # Percent with Health Insurance
             totpop_noins_   = "S2701_C04_001", # Total Civ Population, NO Health Insurance
             pct_uninsured_  = "S2701_C05_001") # Percent with No Health Insurance

alphabeta2  <- tidycensus::get_acs(survey="acs5", year=2020, geography = "county", state = "CA", 
                            county=c(1,13,41,55,75,81,85,95,97),
                            show_call = TRUE, output = "wide",
                            variables = selvars)

####################################################################################

#  Demonstration of tidyverse package purrr to extract multiple years in one call

myvars <- c(total_worker_  = "B08006_001", 
            transit_       = "B08006_008", 
            bicycle_       = "B08006_014", 
            walked_        = "B08006_015",  
            at_home_       = "B08006_017")
            
years <- 2006:2019   # or years5 <- c(2010,2015,2020) for 5-year ACS
                     # or yearsx <- c(2006:2019,2021) for 1-year ACS, skipping 2020
names(years) <- years

### Pull US States for Selected Journey-to-Work Variables, 2006-2019
state1 <- purrr::map_dfr(years, ~{ tidycensus::get_acs(survey = "acs1",
                 geography = "state", 
                 variables = myvars, output='wide',
                 year = .x)}, .id = "year") %>% 
             dplyr::arrange(GEOID,year)
     
################################################################################   

#  Section 3. Census Transportation Planning Package, 2006-2010 or 2012-2016
#

library("CTPPr")            
library('tidyverse')
# Example: Counties within Nevada
Nevada1 <- download_ctpp(A101100, dataset='2016', geography='County', state="Nevada")

# Example: Places within Nevada
Nevada2 <- download_ctpp(A101100, dataset='2016', geography='Place', state="Nevada")

# Example: All Census Tracts within Nevada with the Tract Name
Nevada3 <- download_ctpp(A101100, dataset='2016', geography='Tract', state="Nevada",
                                   output="Name")

# Example: Tracts within Nevada, Using the FIPS Code instead of Name
Nevada4 <- download_ctpp(A101100, dataset='2016', geography='Tract', state="Nevada",
                                   output = 'FIPS Code')

# Example: Tracts within California, Split FIPS Code
# probably need some follow-on examples to split the FIPS code
#  into 3 separate variables: state, county, tract.
Nevada5  <- download_ctpp(A101100, dataset='2016', geography='Tract', state="Nevada",
                                    output = 'Split FIPS Code')

Nevada6  <- download_ctpp(A101100, dataset='2016', geography='Tract', state="Nevada",
                          output = 'FIPS and Name') %>% 
            dplyr::mutate(state =substr(RESIDENCE,1,2),
                          county=substr(RESIDENCE,3,5),
                          tract =substr(RESIDENCE,6,11),
                          name  =substr(RESIDENCE,14,99)) %>% 
            dplyr::relocate(state:name,.before=RESIDENCE)

# Pull a two way table: Population by Hispanic (3) by Race (5)
#  This yields 15 records (3*5) for each County
#  Two Additional columns are included in this particular data frame:
#    -- "Hispanic Origin 3"
#    -- "Race of Person 5"
Nevada7 <- download_ctpp(A101204, dataset = '2016', output = "FIPS and Name",
                           geography = 'County', state = "Nevada")

# Flow Data
start_timer <- Sys.time() # start stopwatch on this function
Nevada8 <- download_ctpp(id="A302100", dataset = '2016', 
                output = "FIPS and Name", geography = 'County->County', state = "Nevada")
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

start_timer <- Sys.time() # start stopwatch on this function
Nevada9 <- download_ctpp(id="A302103", dataset = '2016', 
                         output = "FIPS and Name", geography = 'County->County', state = "Nevada")
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

# Try a pivot_wider with the county-to-county workers by means of transportation
Nevada9a <- Nevada9 %>% 
  pivot_wider(names_from = 
                c("Means of Transportation 18"),
              values_from = Estimate, -SE)

Nevada9b <- Nevada9 %>% 
  pivot_wider(names_from = 
                c("Means of Transportation 18"),
              values_from = SE, -Estimate)

Nevada9c <- dplyr::left_join(Nevada9a,Nevada9b, 
                              by=c("RESIDENCE","WORKPLACE"),
                              suffix = c("_est", "_se"))

#  "Three-Way CTPP Table"
#
#  A112309 = Households by Household Size (5) by
#            Vehicles Available (5) by
#            Tenure (5)

Nevada10  <- download_ctpp(A112309,dataset='2016', geography='Place',
                        state="Nevada", output = 'FIPS and Name')

results1 <- Nevada10 %>% 
  pivot_wider(names_from = 
                c("Household Size 5",
                  "Vehicles Available 5",
                  "Tenure 5"),
              values_from = Estimate, -SE)

results1_se <- Nevada10 %>% 
  pivot_wider(names_from = 
                c("Household Size 5",
                  "Vehicles Available 5",
                  "Tenure 5"),
              values_from = SE, -Estimate)

Nevada10a <- dplyr::left_join(results1,results1_se, 
                              by="RESIDENCE",
                              suffix = c("_est", "_se"))

# Reading in csv file exported from Beyond 2020
library(tidyverse)
setwd("~/Desktop/tidycensus_work")

# Nevada tract-of-work by Means of Transportation
step1 <- readr::read_csv("A202105 - Means of Transportation (18) (Workers 16 years and over).csv",
                         skip=2) %>% 
         dplyr::filter(!is.na(Output)) # deletes the footnote record...

# This produces a clean, wide format table, but with cumbersome/long variable names
step2 <- step1 %>% 
  pivot_wider(names_from=c("Means of Transportation 18","Output"),
              values_from = "Workers 16 and Over")

##########################################################################

# The following example was excluded from the powerpoint presentation....

# Recode the variable names and variable strings to simpler mnemonics
step3 <- step1 %>% 
      rename(means18x="Means of Transportation 18",
             workers ="Workers 16 and Over") %>%
      mutate(Output = recode(Output,
               "Estimate"        = "est",
               "Margin of Error" = "moe")) %>% 
      mutate(means18 = recode(means18x,
              "Total, means of transportation" = "total",
              "Car, truck, or van -- Drove alone" = "drove_alone",
              "Car, truck, or van -- In a 2-person carpool" = "carpool2",
              "Car, truck, or van -- In a 3-person carpool" = "carpool3",
              "Car, truck, or van -- In a 4-person carpool" = "carpool4",
              "Car, truck, or van -- In a 5-or-6-person carpool" = "carpool56",
              "Car, truck, or van -- In a 7-or-more-person carpool" = "carpool7p",
              "Bus or trolley bus" = "bus_trolleybus",
              "Streetcar or trolley car" = "streetcar",
              "Subway or elevated"  = "subway_el",
              "Railroad" = "railroad",
              "Ferryboat" = "ferry",
              "Bicycle" = "bicycle",
              "Walked" = "walked",
              "Taxicab" = "taxicab",
              "Motorcycle" = "motorcycle",
              "Other method" = "other",
              "Worked at home" = "at_home"))

# Short mnemonic variable names for Means of Transportation (18)

step4 <- step3 %>%
  select(-"means18x") %>% 
  pivot_wider(names_from=c("means18","Output"),
              values_from = "workers")

#############################################################

#  Section 4. LEHD/LODES
#

library(lehdr)
library(tidyverse)

flow1 <- grab_lodes(state='nv', year=2019, lodes_type = "od", 
                    job_type = "JT00", # or JT01-Primary Job, etc.
                    agg_geo = "county", # or state, tract, bg or block
                    state_part = "main", # or "aux" -- interstate inbound commuters
                    use_cache = TRUE)

rac1 <- grab_lodes(state='nv', year=2019, lodes_type = "rac", 
                   job_type = "JT00", # or JT01-Primary Job, etc.
                   agg_geo = "county", # or state, tract, bg or block
                  # state_part = "main", # or "aux" -- interstate inbound commuters
                   use_cache = TRUE)

wac1 <- grab_lodes(state='nv', year=2019, lodes_type = "wac", 
                   job_type = "JT00", # or JT01-Primary Job, etc.
                   agg_geo = "county", # or state, tract, bg or block
                #   state_part = "main", # or "aux" -- interstate inbound commuters
                   use_cache = TRUE)

####################################################################################
#  End of Examples Used in Workshop Powerpoint Presentation
####################################################################################