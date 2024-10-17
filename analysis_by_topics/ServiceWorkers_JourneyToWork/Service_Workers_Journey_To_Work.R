# -------------------------------------------------------------------------
#
# This script downloads PUMS data to support BATA team's work 
# to understand the preferred modes of transportation for "service workers" 
# 
# Asana task: https://app.asana.com/0/12291104512646/1208522736825597/f
# 
# -------------------------------------------------------------------------

# Load libraries
library(tidycensus)
library(tidyverse)

# Source external script
USERPROFILE <- Sys.getenv("USERPROFILE")
pums_labels_r_path <- file.path(USERPROFILE, "Documents/GitHub/census-tools-for-planning/labels/pums_labels.R")
source(pums_labels_r_path)

# --------------------------------------------------------------------------------
# Download PUMS data
# --------------------------------------------------------------------------------

# could replace this with a local version of 2023 PUMS on M
# for today, it's quicker for me to just use the API

pums_df <- get_pums(
  state = "CA",
  puma = all_BayAreaPumas_list,
  year = 2023,
  survey = "acs1",
  variables = c(
         "POWSP",      # Place of work state
         "POWPUMA",    # Place of work PUMA
         "JWTRNS",     # Means of transportation to work
         "JWRIP",      # Vehicle occupancy of journey to work
         "POVPIP",     # Income-to-poverty ratio
         "OCCP"        # Occupation
  )
)


# --------------------------------------------------------------------------------
# Add labels
# --------------------------------------------------------------------------------

# Add labels using functions
pums_df <- pums_df %>%
  mutate(
    Home_County = label_home_county(PUMA),
    Work_County = label_work_county(POWPUMA),
    ServiceWorker = label_service_worker(OCCP),
    under_2x_poverty = ifelse(POVPIP <= 200, 1, 0),
    journey_to_work_mode = label_journey_to_work_mode(JWTRNS, JWRIP)
  )

# read in a file about assumed about bridge crossing based on county or PUMS
assumed_bridge_crossing_filepath <- file.path(USERPROFILE, "Documents/GitHub/census-tools-for-planning/analysis_by_topics/ServiceWorkers_JourneyToWork/assumed_bridge_crossing_based_on_county.csv")
assumed_bridge_crossing_df <- read_csv(assumed_bridge_crossing_filepath)

pums_df <- pums_df %>%
  left_join(assumed_bridge_crossing_df, 
            by = c("Home_County" = "Home_County", 
                   "Work_County" = "Work_County")) %>%
  select(-WhichBridge_plusOtherNotes) # Drop the 'WhichBridge_plusOtherNotes' column as it is not needed


# --------------------------------------------------------------------------------
# export the data to csv for easy visualization in Tableau
# --------------------------------------------------------------------------------
# Reordering columns
pums_df <- pums_df %>%
  select(SERIALNO, SPORDER, WGTP, PWGTP, PUMA, Home_County, STATE, JWTRNS, JWRIP, journey_to_work_mode, POWPUMA, 
         Work_County, POWSP, OCCP, Work_County, BridgeCrossing, ServiceWorker, 
         POVPIP, under_2x_poverty)

# somewhere on Box or M?
# M is preferable if we want to have the file read by Tableau
# output_path <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Census_Requests", "BATA_Bridge_Toll_Increase", "PUMS2022_SelectedVars_person.csv")
output_path <- file.path("e:/PUMS2022_SelectedVars_person.csv")
write_csv(pums_df, output_path)


# --------------------------------------------------------------------------------
# Tabulation wthin R
# --------------------------------------------------------------------------------

# version 1 table
# get drive-to-work matrix for those travel by car, by poverty status, by service worker vs non
# -----------------------
DriveToWork_poverty_ServiceWorker_UNweighted_table <- pums_df %>%
  filter(JWTRNS == 1) %>%                    # Takes a car, truck or van to work
  filter(BridgeCrossing == 1) %>%            # Bridge crossing required given home county and work county 
  group_by(Home_County,Work_County,under_2x_poverty, ServiceWorker) %>%
  summarize(count=n()) %>%
  ungroup

DriveToWork_poverty_ServiceWorker_weighted_table <- pums_df %>%
  filter(JWTRNS == 1) %>%                    # Takes a car, truck or van to work
  filter(BridgeCrossing == 1) %>%            # Bridge crossing required given home county and work county 
  group_by(Home_County,Work_County,under_2x_poverty, ServiceWorker) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup

print(DriveToWork_poverty_ServiceWorker_UNweighted_table, , n = Inf)
print(DriveToWork_poverty_ServiceWorker_weighted_table, , n = Inf)

# version 2 table
# get journey-to-work table for those traveling by car and likely to have crossed a bridge, by poverty status, by service worker vs non
# -----------------------

# unweighted
poverty_ServiceWorker_UNweighted_table <- pums_df %>%
  filter(JWTRNS         == 1) %>%                    # Takes a car, truck or van to work
  filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  group_by(under_2x_poverty, ServiceWorker) %>%
  summarize(count=n()) %>%
  ungroup

# weighted
poverty_ServiceWorker_weighted_table <- pums_df %>%
  filter(JWTRNS         == 1) %>%                    # Takes a car, truck or van to work
  filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  group_by(under_2x_poverty, ServiceWorker) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup

print(poverty_ServiceWorker_UNweighted_table, , n = Inf)
print(poverty_ServiceWorker_weighted_table, , n = Inf)


# version 3 table
# focus on mode share of service workers who likely need to cross a bridge for their work trip by poverty level
# -----------------------
JourneyToWorkMode_UNweighted_table <- pums_df %>%
  #filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  filter(ServiceWorker == 'Service_worker') %>%                    
  group_by(under_2x_poverty, journey_to_work_mode) %>%
  summarize(count=n()) %>%
  ungroup

JourneyToWorkMode_weighted_table <- pums_df %>%
  #filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  filter(ServiceWorker == 'Service_worker') %>%                   
  group_by(under_2x_poverty, journey_to_work_mode) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup

print(JourneyToWorkMode_UNweighted_table, , n = Inf)
print(JourneyToWorkMode_weighted_table, , n = Inf)



# version 4 table
# focus on mode share of service workers who likely need to cross a bridge for their work trip by poverty level
# -----------------------
JourneyToWorkMode_UNweighted_table <- pums_df %>%
  #filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  filter(ServiceWorker == 'Service_worker') %>%                    
  group_by(under_2x_poverty, JWTRNS) %>%
  summarize(count=n()) %>%
  ungroup

JourneyToWorkMode_weighted_table <- pums_df %>%
  #filter(BridgeCrossing == 1) %>%                    # Bridge crossing required given home county and work county 
  filter(ServiceWorker == 'Service_worker') %>%                   
  group_by(under_2x_poverty, JWTRNS) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup

print(JourneyToWorkMode_UNweighted_table, , n = Inf)
print(JourneyToWorkMode_weighted_table, , n = Inf)