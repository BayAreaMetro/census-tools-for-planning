# ACS PUMS 2018 Race for Adults 18 and Over.R

# Analyze 2018 Bay Area PUMS data for Bay Area adults by race

suppressMessages(library(tidyverse))
wd <- "M:/Data/HomeInterview/TNC Survey/2020/Draft012220/Analysis/"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pbayarea18.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2018/hbayarea18.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea18 %>%  
  select(SERIALNO,HINCP,ADJINC,NP) %>% 
  mutate(SERIALNO = as.character(SERIALNO))             # Imports as factor; fixing this

# Extract adults, join with household file for income values

adults <- pbayarea18 %>%
  mutate(SERIALNO = as.character(SERIALNO)) %>%         # Imports as factor; fixing this
  filter(AGEP>=18 & RELP<16) %>%                        # Extract only non-gq adults
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
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
      AGEP>=18 & AGEP<=24   ~"1_between 18 and 24",
      AGEP>=25 & AGEP<=34   ~"2_between 25 and 34",
      AGEP>=35 & AGEP<=44   ~"3_between 35 and 44",
      AGEP>=45 & AGEP<=54   ~"4_between 45 and 54",
      AGEP>=55 & AGEP<=64   ~"5_between 55 and 64",
      AGEP>=65              ~"6_65+",
      TRUE                  ~"Uncoded"),
    nprc=case_when(
      NP==1                ~"1 Person HH",
      NP==2                ~"2 Person HH",
      NP==3                ~"3 Person HH",
      NP==4                ~"4 Person HH",
      NP>=5                ~"5+ Person HH"
    )
    )%>%
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,PWGTP,AGEP,JWTR,HINCP,adjustedinc, agerc,incomerc,racerc,nprc,RELP)

# Summarize income

incomesum <- adults %>% 
  group_by(County_Name,incomerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., incomerc, total, fill=0)

# Summarize race

racesum <- adults %>% 
  group_by(County_Name,racerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., racerc, total, fill=0)

# Summarize age

agesum <- adults %>% 
  group_by(County_Name,agerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., agerc, total, fill=0)

# Summarize persons in household

npsum <- adults %>% 
  group_by(County_Name,nprc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  spread (., nprc, total, fill=0)

write.csv(incomesum, "PUMS2018 Adults by Income.csv", row.names = FALSE, quote = T)
write.csv(racesum, "PUMS2018 Adults by Race.csv", row.names = FALSE, quote = T)
write.csv(agesum, "PUMS2018 Adults by Age.csv", row.names = FALSE, quote = T)
write.csv(npsum, "PUMS2018 Adults by HH Size.csv", row.names = FALSE, quote = T)


