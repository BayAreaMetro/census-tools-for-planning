# ACS 2015 PUMS HHs with 4 plus autos.R
# Analyze PUMS data for mean number of autos in HHs with 4+ autos
# 2015 1-year PUMS data

# Import Libraries

suppressMessages(library(tidyverse))
library(questionr)

# Input HH PUMS file and set working directory

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015/hbayarea15.Rdata" 
WD = "m:/data/requests/Flavia Tsang"                       # If later needed for any output
setwd(WD)

# Bring in HH and person data

load (HH_RDATA) 

# Recode data

household <- hbayarea15 %>% 
  filter(VEH>=4) %>% 
  summarize(average4p=weighted.mean(VEH,WGTP))

print(household)
