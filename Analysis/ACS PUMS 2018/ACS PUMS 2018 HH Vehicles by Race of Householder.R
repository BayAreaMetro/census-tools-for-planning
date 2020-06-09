# ACS PUMS 2018 HH Vehicles by Race of Householder.R

suppressMessages(library(tidyverse))
wd <- "M:/Data/Requests/Raleigh McCoy"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pbayarea18.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2018/hbayarea18.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea18 %>%  
  select(SERIALNO,VEH,TEN,WGTP) %>% 
  filter(!is.na(TEN)) %>% 
  mutate(SERIALNO = as.character(SERIALNO))             # Imports as factor; fixing this

# Extract adults, join with household file for income values

persons <- pbayarea18 %>%
  mutate(SERIALNO = as.character(SERIALNO)) %>%         # Imports as factor; fixing this
  filter(RELP==0) %>%                                   # Householder only
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
    vehrc=case_when(
      VEH==0             ~ "0_Vehicle",
      VEH>=1             ~ "1p_Vehicle",
      TRUE               ~ "Not coded"
    ),
    racerc=case_when(
      HISP>1               ~"5_Hispanic",
      HISP==1 & RAC1P==1   ~"1_White",
      HISP==1 & RAC1P==2   ~"2_Black",
      HISP==1 & RAC1P==3   ~"4_Other",
      HISP==1 & RAC1P==4   ~"4_Other",
      HISP==1 & RAC1P==5   ~"4_Other",
      HISP==1 & RAC1P==6   ~"3_Asian",
      HISP==1 & RAC1P>=7   ~"4_Other",
      TRUE                 ~"Uncoded")) %>%
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,PWGTP,WGTP,racerc,RELP,vehrc)

# Vehicles by county and race of householder

final <- persons %>% 
  group_by(County_Name,racerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., racerc, total, fill=0)

write.csv(final, "ACS PUMS2018 HH Vehicles by Race of Householder.csv", row.names = FALSE, quote = T)
