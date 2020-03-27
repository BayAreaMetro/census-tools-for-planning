# ACS PUMS 2018 Income Age by Vehicle Availability.R

# Analyze 2018 Bay Area PUMS data for Bay Area HH vehicles availability by income and age of householder

suppressMessages(library(tidyverse))
library(writexl)
wd <- "M:/Data/Requests/Rebecca Long/PUMS 2018 Vehicle Summaries/"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pbayarea18.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2018/hbayarea18.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

hbayarea18 <- hbayarea18 %>% 
  mutate(SERIALNO=as.character(SERIALNO)) %>% 
  filter(NP!=0 & TYPE==1)                               # Remove vacant units, GQ from households

pbayarea18 <- pbayarea18 %>% 
  select(-ADJINC,-County_Name) %>%       # Remove from person file, use HH version upon join
  mutate(SERIALNO=as.character(SERIALNO)) %>% 
  filter(SPORDER==1 & RELP<16)           # Extract only householder and remove GQ

combined <- left_join(hbayarea18,pbayarea18, by="SERIALNO") %>% 
  select(SERIALNO,HINCP,ADJINC,SPORDER,County_Name,VEH,AGEP, WGTP) %>% mutate(
    adjustedinc=HINCP*(ADJINC/1000000),
    incomerc=case_when(
      adjustedinc <25000                          ~"1_less than 25k",
      adjustedinc >=25000 & adjustedinc <50000    ~"2_25k to 49,999",
      adjustedinc >=50000 & adjustedinc <100000   ~"3_50k to 99,999",
      adjustedinc >=100000 & adjustedinc <150000  ~"5_100k to 149,999",
      adjustedinc >=150000                        ~"6_150k+",
      TRUE                                        ~"Uncoded, group quarters"),
    agerc=case_when(
      AGEP<25               ~"0_less than 25",
      AGEP>=25 & AGEP<=34   ~"1_between 25 and 34",
      AGEP>=35 & AGEP<=54   ~"2_between 35 and 54",
      AGEP>=55 & AGEP<=64   ~"3_between 55 and 64",
      AGEP>=65              ~"4_65+",
      TRUE                  ~"Uncoded"),
    vehrc=case_when(
      VEH==0                ~"0 car HH",
      VEH==1                ~"1 car HH",
      VEH==2                ~"2 car HH",
      VEH==3                ~"3 car HH",
      VEH>=4                ~"4+ car HH"
    ))

# Summarize income

incomesum <- combined %>% 
  group_by(County_Name,incomerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., incomerc, total, fill=0)

incomesum_bay <- combined %>% 
  group_by(incomerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., incomerc, total, fill=0)


# Summarize age

agesum <- combined %>% 
  group_by(County_Name,agerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., agerc, total, fill=0)

agesum_bay <- combined %>% 
  group_by(agerc,vehrc) %>% 
  summarize(total=sum(WGTP)) %>% 
  spread (., agerc, total, fill=0)


write.csv(incomesum, "PUMS2018 County Vehicles by Income.csv", row.names = FALSE, quote = T)
write.csv(incomesum_bay, "PUMS2018 Bay Vehicles by Income.csv", row.names = FALSE, quote = T)
write.csv(agesum, "PUMS2018 County Vehicles by Age.csv", row.names = FALSE, quote = T)
write.csv(agesum_bay, "PUMS2018 Bay Vehicles by Age.csv", row.names = FALSE, quote = T)


