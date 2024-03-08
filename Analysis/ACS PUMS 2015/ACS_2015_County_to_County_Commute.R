# ACS_2015_County_to_County_Commute.R
# Extract journey to work flow for 2015
# Import Libraries

suppressMessages(library(tidyverse))

# Set working directory

wd <- "M:/Data/Requests/Flavia Tsang"
setwd(wd)

# Input person file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2015/pbayarea15.Rdata"
baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700)

load (PERSON_RDATA)

workers <- pbayarea15 %>% 
  filter(POWPUMA %in% baypowpuma & POWSP==6) %>% 
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
  )) 

# Summarize total workers, reformat to wide format, and export

worker_summary <- workers %>%
  group_by(County_Name,Work_County) %>%
  summarize(worker_total=sum(PWGTP)) %>%
  ungroup %>% 
  pivot_wider(names_from="Work_County",values_from="worker_total",values_fill=0)

write.csv(worker_summary, "ACS PUMS 2015 county to county commuters.csv", row.names = FALSE, quote = T)

