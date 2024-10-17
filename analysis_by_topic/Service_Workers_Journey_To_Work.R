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
         "ESR",        # Employment status recode
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
    Work_County = label_work_county(POWPUMA,POWSP),
    ServiceWorker = label_service_worker(OCCP),
    under_2x_poverty = ifelse(POVPIP <= 200, 1, 0),
    journey_to_work_mode = label_journey_to_work_mode(JWTRNS, JWRIP)
  )



# --------------------------------------------------------------------------------
# export the data to csv for easy visualization in Tableau
# --------------------------------------------------------------------------------
# Reordering columns
pums_df <- pums_df %>%
  select(SERIALNO, SPORDER, WGTP, PWGTP, ESR, PUMA, Home_County, STATE, JWTRNS, JWRIP, journey_to_work_mode, POWPUMA, 
         Work_County, POWSP, OCCP, Work_County, ServiceWorker, POVPIP, under_2x_poverty)

# somewhere on Box or M?
# M is preferable if we want to have the file read by Tableau
# output_path <- file.path(USERPROFILE, "Box", "Modeling and Surveys","Census_Requests", "BATA_Bridge_Toll_Increase")
output_path <- file.path("M:/Data/Requests/BATA_Bridge_Toll_Increase")
write_csv(pums_df, file.path(output_path,"PUMS2022_SelectedVars_person.csv"))


# Write a log file
timestamp <- Sys.time()
log_entry <- paste("Data saved on:", timestamp, 
                   "\nSource script: https://github.com/BayAreaMetro/census-tools-for-planning/tree/master/analysis_by_topic/Service_Workers_Journey_To_Work.R", 
                   "\nAsana task: https://app.asana.com/0/12291104512646/1208522736825597/f", 
                   "\nSaved file path:", file.path(output_path, "PUMS2022_SelectedVars_person.csv"), "\n\n")
write(log_entry, file = file.path(output_path,"log.txt"), append = TRUE)

# --------------------------------------------------------------------------------
# Tabulation wthin R
# --------------------------------------------------------------------------------

# Table 1
# Percent of service workers
ServiceWorker_UNweighted_table <- pums_df %>%
  group_by(ServiceWorker) %>%
  summarize(count=n()) %>%
  ungroup

ServiceWorker_weighted_table <- pums_df %>%
  group_by(ServiceWorker) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup

print(ServiceWorker_UNweighted_table, , n = Inf)
print(ServiceWorker_weighted_table, , n = Inf)


# Table 2
# get drive-to-work matrix for those travel by car, by poverty status, by service worker vs non
# -----------------------
DriveToWork_poverty_ServiceWorker_UNweighted_table <- pums_df %>%
  filter(JWTRNS == 1) %>%                    # Takes a car, truck or van to work
  group_by(under_2x_poverty,ServiceWorker, Home_County,Work_County) %>%
  summarize(count=n()) %>%
  ungroup %>% 
  spread(Work_County, count, fill = 0)

DriveToWork_poverty_ServiceWorker_weighted_table <- pums_df %>%
  filter(JWTRNS == 1) %>%                    # Takes a car, truck or van to work
  group_by(under_2x_poverty,ServiceWorker, Home_County,Work_County) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup %>% 
  spread(Work_County, worker_total, fill = 0)

print(DriveToWork_poverty_ServiceWorker_UNweighted_table, , n = Inf)
print(DriveToWork_poverty_ServiceWorker_weighted_table, , n = Inf)

# Table 3
# Journey to work mode by poverty status and by service worker vs non
# -----------------------

JourneyToWorkMode_poverty_ServiceWorker_UNweighted_table <- pums_df %>%
  group_by(under_2x_poverty,ServiceWorker, journey_to_work_mode) %>%
  summarize(count=n()) %>%
  ungroup

JourneyToWorkMode_poverty_ServiceWorker_weighted_table <- pums_df %>%
  group_by(under_2x_poverty,ServiceWorker, journey_to_work_mode) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup 

print(JourneyToWorkMode_poverty_ServiceWorker_UNweighted_table, , n = Inf)
print(JourneyToWorkMode_poverty_ServiceWorker_weighted_table, , n = Inf)

