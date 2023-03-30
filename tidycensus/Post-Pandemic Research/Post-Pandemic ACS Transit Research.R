# Post-Pandemic ACS Transit Research.R
# Research differences in transit ridership 2019 and 2021


library (tidyverse)
library (tidycensus)

baycounties          <- c("01","13","41","55","75","81","85","95","97")
censuskey            <- readLines("M:/Data/Census/API/api-key.txt")
place_eq_in          <- "M:/Data/Census/corrlib/Census2010/Census 2010_Places_65k_Over.csv"
place_eq             <- read.csv(place_eq_in,header = T)

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_TM               <- file.path(USERPROFILE, "Box", "Modeling and Surveys")
PBA_TAZ_2010         <- file.path(BOX_TM, "Share Data",   "plan-bay-area-2040", "2010_06_003","tazData.csv")


acs_variables <- c(total_pop_              ="B01003_001",          # Total area population
                   total_transit_          ="C08301_008",          # Total transit users
                   total_commuters_        ="C08301_001",          # Total commuters
                   total_wfh_              ="C08301_011",          # Total work from home (for calculating out of home modes)
                   transit_wnh_            ="B08105H_004",         # White, not Hispanic transit use
                   transit_female_         ="C08006_032",          # Female transit user
                   transit_native_         ="C08111_017",          # Native public transit user
                   transit_naturalized_    ="C08111_019",          # Naturalized public transit user
                   transit_notcitizen_     ="C08111_020",          # Not a citizen public transit user
                   #commute_poverty_tot_   ="B08122_001",          # Poverty commuter total is different from full universe
                   #transit_poverty_tot_   ="B08122_013",          # Poverty transit user total is different from full universe
                   #transit_poverty_lt_    ="B08122_014",          # Transit user below 100 percent poverty
                   commute_tot_earnings_   ="C08119_001",          # Total commuters for earnings is different from full universe
                   transit_tot_earnings_   ="C08119_028",          # Transit commuters for earnings is different from full universe
                   transit_earnings_50_65_ ="C08119_034",          # Transit commuters with earnings 50-65
                   transit_earnings_65_75_ ="C08119_035",          # Transit commuters with earnings 65-75
                   transit_earnings_75p_   ="C08119_036",          # Transit commuters with earnings 75+       
                   transit_wah_occ_        ="C08124_023",          # Transit work at home occs - mgmt.,bus.,science,arts
                   transit_hh_             ="B08137_010",          # Transit users in households differs from full universe
                   transit_rent_           ="B08137_012",          # Transit user rent home
                   transit_0_veh_          ="C08141_017",          # Transit user zero vehicle households
                   transit_med_age_        ="B08103_004",          # Median age for transit users
                   transit_LEP_            ="C08113_020"           # Transit users speak English less than very well
)
  
# 2019 data

places_19 <- get_acs(geography = "place", variables = acs_variables,
                     state = "06", 
                     year=2019,
                     output="wide",
                     survey = "acs1",
                     key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME") %>% 
  slice_max(total_pop_E,n=10)

counties_19 <- get_acs(geography = "county", variables = acs_variables,
                       state = "06", county = baycounties,
                       year=2019,
                       output="wide",
                       survey = "acs1",
                       key = censuskey) %>% 
  arrange(NAME) 

bay_19 <- counties_19 %>% 
  select(3:ncol(.)) %>% 
  summarise_all (sum) %>% 
  mutate(GEOID="99999",NAME="Bay Area",transit_med_age_E=NA_real_)


# 2021 data                 

places_21 <- get_acs(geography = "place", variables = acs_variables,
          state = "06", 
          year=2021,
          output="wide",
          survey = "acs1",
          key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME") %>% 
  slice_max(total_pop_E,n=10)

counties_21 <- get_acs(geography = "county", variables = acs_variables,
                  state = "06", county = baycounties,
                  year=2021,
                  output="wide",
                  survey = "acs1",
                  key = censuskey) %>% 
  arrange(NAME) 

bay_21 <- counties_21 %>% 
  select(3:ncol(.)) %>% 
  summarise_all (sum) %>% 
  mutate(GEOID="99999",NAME="Bay Area",transit_med_age_E=NA_real_)


# Combine and remove margin of error field
combined_19 <- rbind(counties_19,places_19,bay_19) %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~str_remove(., '_E')) %>% 
  rename_with(~paste0(.,"_2019"),3:ncol(.)) %>% 
  filter(!(is.na(GEOID)))
  
combined_21 <- rbind(counties_21,places_21,bay_21) %>% 
  select(!(ends_with("_M"))) %>% 
  rename_with(~str_remove(., '_E')) %>% 
  rename_with(~paste0(.,"_2021"),3:ncol(.)) 

final <- left_join(combined_19,combined_21,by=c("GEOID","NAME")) %>% 
  select(sort(names(.))) %>% 
  relocate(c("GEOID","NAME"),.before = commute_tot_earnings_2019) %>% 
  mutate(transit_earnings_50p_2019=transit_earnings_50_65_2019+transit_earnings_65_75_2019+transit_earnings_75p_2019,
         transit_earnings_50p_2021=transit_earnings_50_65_2021+transit_earnings_65_75_2021+transit_earnings_75p_2021)

