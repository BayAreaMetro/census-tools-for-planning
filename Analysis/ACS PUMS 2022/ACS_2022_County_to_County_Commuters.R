# ACS_2022_County_to_County_Commuters.R

# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# Set working directory

wd <- "M:/Data/Requests/Flavia Tsang"
setwd(wd)

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

baypowpuma = c("00100","01300","04100","05500","07500","08100","08500","09500","09700")

# Script to extract 2022 persons 
# Recode residence and work counties

pbayarea22 <- get_pums(
  variables = c("PUMA","POWPUMA","POWSP"),
  survey = "acs1",
  state = "CA",
  year = 2022,
  recode = TRUE) %>% 
  filter(PUMA %in% baypuma) %>% 
  mutate(County_Name=case_when(
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

workers <- pbayarea22 %>% 
  filter(POWPUMA %in% baypowpuma & POWSP=="006") %>% 
  mutate(Work_County=case_when(
    POWPUMA=="00100"   ~"Alameda",
    POWPUMA=="01300"   ~"Contra Costa",
    POWPUMA=="04100"   ~"Marin",
    POWPUMA=="05500"   ~"Napa",
    POWPUMA=="07500"   ~"San Francisco",
    POWPUMA=="08100"   ~"San Mateo",
    POWPUMA=="08500"   ~"Santa Clara",
    POWPUMA=="09500"   ~"Solano",
    POWPUMA=="09700"   ~"Sonoma"
  )) 

# Summarize total workers, reformat to wide format, and export

worker_summary <- workers %>%
  group_by(County_Name,Work_County) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup %>% 
  pivot_wider(names_from="Work_County",values_from="worker_total",values_fill=0)

write.csv(worker_summary, "ACS PUMS 2022 county to county commuters.csv", row.names = FALSE, quote = T)
