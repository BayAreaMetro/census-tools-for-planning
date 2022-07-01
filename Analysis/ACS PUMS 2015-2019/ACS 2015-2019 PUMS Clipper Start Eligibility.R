# ACS 2015-2019 PUMS Clipper Start Eligibility.R


# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# Create vector of Bay Area PUMAs

baypuma    <- c("00101", "00102", "00103", "00104", "00105", "00106", "00107", "00108", "00109", "00110", 
                "01301", "01302", "01303", "01304", "01305", "01306", "01307", "01308", "01309", "04101", 
                "04102", "05500", "07501", "07502", "07503", "07504", "07505", "07506", "07507", "08101", 
                "08102", "08103", "08104", "08105", "08106", "08501", "08502", "08503", "08504", "08505", 
                "08506", "08507", "08508", "08509", "08510", "08511", "08512", "08513", "08514", "09501", 
                "09502", "09503", "09701", "09702", "09703")

# Script to extract 2019 california commuters by mode, 
# including recoded variable names and replicate weights for standard error calculation

bay_transit <- get_pums(
  variables = c("PUMA","JWTRNS", "RAC1P", "HISP", "POVPIP"),
                survey = "acs5",
                state = "CA",
                year = 2019,
                variables_filter = list(JWTRNS=02:06),           # Only people who take transit
                recode = TRUE) %>% 
  filter(PUMA %in% baypuma) %>% 
  mutate(
    poverty_recoded=case_when(
      POVPIP<200              ~ "Less than 200",
      POVPIP>=200             ~ "200+",
      TRUE                    ~ "Miscoded"),
    race_recoded=case_when(
      HISP!="01"                                          ~ "5_Hispanic",
      HISP=="01" & RAC1P=="1"                             ~ "1_White",
      HISP=="01" & RAC1P=="2"                             ~ "2_Black",
      HISP=="01" & RAC1P %in% c("3","4","5","7","8","9")  ~ "4_Other",
      HISP=="01" & RAC1P=="6"                             ~ "3_Asian",
      TRUE                                                ~ "Miscoded")
    ) %>% 
  group_by(poverty_recoded,race_recoded) %>% 
  summarize(total=sum(PWGTP))
  
# Export the data

write.csv(bay_transit,file = "M:/Data/Requests/Lysa Hale/Clipper Start/Poverty and Race for Transit Commuters.csv",row.names = F)
