# ACS 2013-2017 outcommute.R
# Analyze PUMS data for Bay Area outcommuters 2017 5-year PUMS data
# Import Libraries

suppressMessages(library(tidyverse))

# Set working directory

wd <- "C:/Users/sisrael/Documents/GitHub/petrale/applications/travel_model_lu_inputs/2015/Employment/Incommute/"
setwd(wd)

# Input person census files for workers working in the Bay, read in industry equivalency

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pbayarea1317.Rdata"
baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # Bay Area counties
california = 6                                              # State code for CA

load (PERSON_RDATA)

# Sum and print outcommuters

outcommuters <- pbayarea1317 %>%                            
  filter((POWSP!=california |                                # Commuters who either work outside CA or
            !(POWPUMA %in% baypowpuma))&                     # in other counties (POWPUMAs than the Bay Area)
           !is.na(POWPUMA)                                   # but are workers (don't have NA for work PUMA)
           ) %>% 
  select(PUMA,PUMA_Name,County_Name,PWGTP,POWPUMA,POWSP) 

outcommuters %>% 
  summarize(total=sum(PWGTP)) %>% 
  print
