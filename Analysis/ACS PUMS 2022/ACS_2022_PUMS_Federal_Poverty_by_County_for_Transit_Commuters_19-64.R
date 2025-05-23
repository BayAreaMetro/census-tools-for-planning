# ACS 2022 PUMS Federal Poverty by County for Transit Commuters 19-64.R

# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# Create vector of Bay Area PUMAs

baypuma       <- c("00101","00111","00112","00113","00114","00115","00116","00117","00118","00119","00120","00121","00122",
                   "00123","01301","01305","01308","01309","01310","01311","01312","01313","01314","04103","04104", "05500",
                   "07507","07508","07509","07510","07511","07512","07513","07514","08101","08102","08103","08104","08105","08106",
                   "08505","08506","08507","08508","08510","08511","08512","08515","08516","08517","08518","08519","08520","08521",
                   "08522","09501", "09502", "09503","09702","09704","09705","09706")

alameda       <- c("00101","00111","00112","00113","00114","00115","00116","00117","00118","00119","00120","00121","00122","00123")
contra_costa  <- c("01301","01305","01308","01309","01310","01311","01312","01313","01314")
marin         <- c("04103","04104")
napa          <- c("05500")
san_francisco <- c("07507","07508","07509","07510","07511","07512","07513","07514")
san_mateo     <- c("08101","08102","08103","08104","08105","08106")
santa_clara   <- c("08505","08506","08507","08508","08510","08511","08512","08515","08516","08517","08518","08519","08520","08521","08522")
solano        <- c("09501", "09502", "09503")
sonoma        <- c("09702","09704","09705","09706")

# Script to extract 2022 persons by poverty
# including recoded variable names 
# Only include people 19-64

bay_poverty <- get_pums(
  variables = c("PUMA","RAC1P", "HISP", "POVPIP", "AGEP", "JWTRNS"),
  survey = "acs1",
  state = "CA",
  year = 2022,
  recode = TRUE) %>% 
  filter(PUMA %in% baypuma,AGEP>=19 & AGEP<=64,JWTRNS>=2 & JWTRNS<=6) %>% 
  mutate(county=case_when(
    PUMA %in% alameda                   ~ "Alameda",
    PUMA %in% contra_costa              ~ "Contra Costa",
    PUMA %in% marin                     ~ "Marin",
    PUMA %in% napa                      ~ "Napa",
    PUMA %in% san_francisco             ~ "San Francisco",
    PUMA %in% san_mateo                 ~ "San Mateo",
    PUMA %in% santa_clara               ~ "Santa Clara",
    PUMA %in% solano                    ~ "Solano",
    PUMA %in% sonoma                    ~ "Sonoma",
    TRUE                                ~ "Miscoded"
  ))

final <- bay_poverty %>% 
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
  group_by(county, poverty_recoded,race_recoded) %>% 
  summarize(total=sum(PWGTP))

# Export the data

write.csv(final,file = "M:/Data/Requests/Lysa Hale/Clipper Start/Poverty and Race for Transit Commuters 19-64 by county 030824.csv",row.names = F)
