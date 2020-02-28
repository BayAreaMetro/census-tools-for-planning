# ACS PUMS 2014-2018 Bike Mode Share by Income Race and Age.R

# Analyze 2013-2017 Bay Area PUMS data for Bay Area bicycle commuters by income, race, and age

suppressMessages(library(tidyverse))
wd <- "M:/Data/Requests/John Goodwin/Bicycle Commuter Demographics/"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2014-18/pbayarea1418.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2014-18/hbayarea1418.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea1418 %>%  
  select(SERIALNO,HINCP,ADJINC) %>% 
  mutate(SERIALNO = as.character(SERIALNO))             # Imports as factor; fixing this

# Extract bike commuters and recode relevant variables, join with household file for income values

cyclists <- pbayarea1418 %>%
  mutate(SERIALNO = as.character(SERIALNO)) %>%         # Imports as factor; fixing this
  filter(JWTR==9) %>%                                   # Extract only bike commuters
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
    adjustedinc=HINCP*(ADJINC/1000000),
    incomerc=case_when(
      adjustedinc <25000                         ~"1_less to 25k",
      adjustedinc >=25000 & adjustedinc <35000   ~"2_25k to 34,999",
      adjustedinc >=35000 & adjustedinc <50000   ~"3_35k to 49,999",
      adjustedinc >=50000 & adjustedinc <75000   ~"4_50k to 74,999",
      adjustedinc >=75000 & adjustedinc <100000  ~"5_75k to 99,999",
      adjustedinc >=100000 & adjustedinc <150000 ~"6_100k to 149,999",
      adjustedinc >=150000 & adjustedinc <250000 ~"7_150k to 249,999",
      adjustedinc >= 250000                       ~"8_250k+",
      TRUE                                       ~"Uncoded, group quarters"),
    racerc=case_when(
      HISP>1               ~"5_Hispanic",
      HISP==1 & RAC1P==1   ~"1_White",
      HISP==1 & RAC1P==2   ~"2_Black",
      HISP==1 & RAC1P==3   ~"4_Other",
      HISP==1 & RAC1P==4   ~"4_Other",
      HISP==1 & RAC1P==5   ~"4_Other",
      HISP==1 & RAC1P==6   ~"3_Asian",
      HISP==1 & RAC1P>=7   ~"4_Other",
      TRUE                 ~"Uncoded"),
    agerc=case_when(
      AGEP<20              ~"1_less_than_20",
      AGEP>=20 & AGEP<30   ~"2_between 20 and 30",
      AGEP>=30 & AGEP<40   ~"3_between 30 and 40",
      AGEP>=40 & AGEP<50   ~"4_between 40 and 50",
      AGEP>=50 & AGEP<60   ~"5_between 50 and 60",
      AGEP>=60 & AGEP<70   ~"6_between 60 and 70",
      AGEP>=70             ~"7_70+",
      TRUE                 ~"Uncoded")
    )%>% 
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,PWGTP,AGEP,JWTR,HINCP,adjustedinc, agerc,incomerc,racerc,RELP)

# Summarize income

incomesum <- cyclists %>% 
  group_by(County_Name,incomerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., incomerc, total, fill=0)

# Summarize race

racesum <- cyclists %>% 
  group_by(County_Name,racerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., racerc, total, fill=0)

# Summarize age

agesum <- cyclists %>% 
  group_by(County_Name,agerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., agerc, total, fill=0)

write.csv(incomesum, "PUMS2014-2018 Bike Commuters by Income.csv", row.names = FALSE, quote = T)
write.csv(racesum, "PUMS2014-2018 Bike Commuters by Race.csv", row.names = FALSE, quote = T)
write.csv(agesum, "PUMS2014-2018 Bike Commuters by Age.csv", row.names = FALSE, quote = T)


