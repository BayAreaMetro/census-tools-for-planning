# ACS_2015_2022_County_to_County_Commuters_By_Mode.R

# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# Set working directory

wd <- "M:/Data/Requests/Flavia Tsang"
setwd(wd)

# Input person file for 2015

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2015/pbayarea15.Rdata"
baypowpuma15 = c(100,1300,4100,5500,7500,8100,8500,9500,9700)

load (PERSON_RDATA)

workers15 <- pbayarea15 %>% 
  filter(POWPUMA %in% baypowpuma15 & POWSP==6) %>% 
  mutate(Work_County=case_when(
    POWPUMA==100    ~"Alameda",
    POWPUMA==1300   ~"Contra Costa",
    POWPUMA==4100   ~"Marin",
    POWPUMA==5500   ~"Napa",
    POWPUMA==7500   ~"San Francisco",
    POWPUMA==8100   ~"San Mateo",
    POWPUMA==8500   ~"Santa Clara",
    POWPUMA==9500   ~"Solano",
    POWPUMA==9700   ~"Sonoma"
  ),Year=2015) %>% 
  rename(Residence_County=County_Name) %>% 
  select(Residence_County,Work_County,JWTR_JWTRNS=JWTR,PWGTP,Year)

# Create vector of Bay Area PUMAs for 2022

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

baypowpuma22 = c("00100","01300","04100","05500","07500","08100","08500","09500","09700")

# Script to extract 2022 persons 
# Recode residence and work counties

pbayarea22 <- get_pums(
  variables = c("PUMA","POWPUMA","POWSP","JWTRNS"),
  survey = "acs1",
  state = "CA",
  year = 2022,
  recode = TRUE) %>% 
  filter(PUMA %in% baypuma) %>% 
  mutate(Residence_County=case_when(
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

workers22 <- pbayarea22 %>% 
  filter(POWPUMA %in% baypowpuma22 & POWSP=="006") %>% 
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
  ),Year=2022) %>% 
  rename(JWTR_JWTRNS=JWTRNS) %>% 
  select(names(workers15)) 

# Bind datasets and export

final <- rbind(workers15,workers22)

write.csv(final, "ACS PUMS 2015 and 2022 county to county commuters by mode.csv", row.names = FALSE, quote = T)
