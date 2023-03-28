library (tidyverse)
library (tidycensus)

baycounties          <- c("01","13","41","55","75","81","85","95","97")
censuskey            <- readLines("M:/Data/Census/API/api-key.txt")
place_eq_in          <- "M:/Data/Census/corrlib/Census2010/Census 2010_Places_65k_Over.csv"
place_eq             <- read.csv(place_eq_in,header = T)

acs_variables <- c(total_pop_              ="B01003_001",          # Total area population
                   transit_tot_            ="C08301_008",          # Total transit users
                   transit_wnh_            ="B08105H_004",         # White, not Hispanic transit use
                   transit_native_         ="C08111_017",          # Native public transit user
                   transit_naturalized_    ="C08111_019",          # Naturalized public transit user
                   transit_notcitizen_     ="C08111_020",          # Not a citizen public transit user
                   transit_lt_poverty_     ="C08122_014",          # Transit user below 100 percent poverty
                   transit_wah_occ_        ="C08111_020",          # Transit work at home occs - mgmt.,bus.,science,arts
                   transit_own_            ="B08137_011",          # Transit user own home
                   transit_rent_           ="B08137_012",          # Transit user rent home
                   transit_0_veh_          ="C08141_017"           # Transit user zero vehicle households
)
  
# 2019 data

places_19 <- get_acs(geography = "place", variables = acs_variables,
                     state = "06", 
                     year=2019,
                     output="wide",
                     survey = "acs1",
                     key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME")  

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
  mutate(GEOID="99999",NAME="Bay Area")


# 2021 data                 

places_21 <- get_acs(geography = "place", variables = acs_variables,
          state = "06", 
          year=2021,
          output="wide",
          survey = "acs1",
          key = censuskey) %>% 
  arrange(NAME) %>% 
  left_join(place_eq,.,by="NAME")

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
  mutate(GEOID="99999",NAME="Bay Area")


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

final <- left_join(combined_19,combined_21,by=c("GEOID","NAME"))

