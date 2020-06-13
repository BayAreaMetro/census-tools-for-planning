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
  select(SERIALNO,VEH,TEN,WGTP,HINCP,ADJINC) %>% 
  filter(!is.na(TEN)) %>% 
  mutate(SERIALNO = as.character(SERIALNO))             # Imports as factor; fixing this

# Join person file with household file for number of vehicles

persons <- pbayarea18 %>%
  mutate(SERIALNO = as.character(SERIALNO)) %>%         # Imports as factor; fixing this
  filter(RELP==0) %>%                                   # Retain householder only
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
    vehrc=case_when(                                    # Recode number of vehicles
      VEH==0             ~ "0_Vehicle",
      VEH>=1             ~ "1p_Vehicle",
      TRUE               ~ "Not coded"
    ),
    adjustedinc=HINCP*(ADJINC/1000000),
    incomerc=case_when(
      adjustedinc <25000                         ~"1_less to 25k",
      adjustedinc >=25000 & adjustedinc <50000   ~"2_25k to 49,999",
      adjustedinc >=50000 & adjustedinc <75000   ~"3_50k to 74,999",
      adjustedinc >=75000 & adjustedinc <100000  ~"4_75k to 99,999",
      adjustedinc >=100000 & adjustedinc <150000 ~"5_100k to 149,999",
      adjustedinc >=150000 & adjustedinc <200000 ~"6_150k to 199,999",
      adjustedinc >=200000 & adjustedinc <250000 ~"7_200k to 249,999",
      adjustedinc >=250000                       ~"8_250k+",
      TRUE                                       ~"Uncoded, group quarters"),
    racerc=case_when(                                   # Recode race of householder
      HISP>1               ~"5_Hispanic",
      HISP==1 & RAC1P==1   ~"1_White",
      HISP==1 & RAC1P==2   ~"2_Black",
      HISP==1 & RAC1P==3   ~"4_Other",
      HISP==1 & RAC1P==4   ~"4_Other",
      HISP==1 & RAC1P==5   ~"4_Other",
      HISP==1 & RAC1P==6   ~"3_Asian",
      HISP==1 & RAC1P>=7   ~"4_Other",
      TRUE                 ~"Uncoded")) %>%
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,PWGTP,WGTP,racerc,RELP,vehrc,adjustedinc,HINCP,incomerc)

# Vehicles by county and race of householder, output file

race <- persons %>% 
  group_by(County_Name,racerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., racerc, total, fill=0)

income <- persons %>% 
  group_by(County_Name,incomerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., incomerc, total, fill=0)

write.csv(race, "ACS PUMS2018 HH Vehicles by Race of Householder.csv", row.names = FALSE, quote = T)
write.csv(income, "ACS PUMS2018 HH Vehicles by Household Income.csv", row.names = FALSE, quote = T)
