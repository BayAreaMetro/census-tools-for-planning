# ACS_PUMS_2023_Smartphones_HouseholderRace_Income_Size.R

# Library

suppressMessages(library(tidyverse))

# Location of files 

userprofile          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
box_location         <- file.path(userprofile, "Box", "Modeling and Surveys")
output               <- file.path(box_location,"Surveys","Travel Diary Survey","BATS_2025_2026","BATS_25_26_Partners_Shared","BATS 25-26 Planning Sprint")

# Input household and person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2023/pbayarea23.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2023/hbayarea23.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea %>%  
  select(SERIALNO,TEN,WGTP,HINCP,ADJINC,NP,SMARTPHONE) %>% 
  filter(!is.na(TEN)) %>%                               # Remove GQ/vacant records
  mutate(SERIALNO = as.character(SERIALNO))             # Imports as factor; fixing this

# Join person file with household file for number of vehicles

combined <- pbayarea %>%
  mutate(SERIALNO = as.character(SERIALNO)) %>%         # Imports as factor; fixing this
  filter(RELSHIPP==20) %>%                                   # Retain householder only
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
    adjustedinc=HINCP*(ADJINC/1000000),
    incomerc=case_when(
      adjustedinc <25000                         ~"less to 25k",
      adjustedinc >=25000 & adjustedinc <50000   ~"25k to 49,999",
      adjustedinc >=50000 & adjustedinc <75000   ~"50k to 74,999",
      adjustedinc >=75000 & adjustedinc <100000  ~"75k to 99,999",
      adjustedinc >=100000 & adjustedinc <150000 ~"100k to 149,999",
      adjustedinc >=150000 & adjustedinc <200000 ~"150k to 199,999",
      adjustedinc >=200000 & adjustedinc <250000 ~"200k to 249,999",
      adjustedinc >=250000                       ~"250k+",
      TRUE                                       ~"Uncoded, group quarters"),
    racerc=case_when(                                   # Recode race of householder
      HISP>1               ~"Hispanic",
      HISP==1 & RAC1P==1   ~"White",
      HISP==1 & RAC1P==2   ~"Black",
      HISP==1 & RAC1P==3   ~"Other",
      HISP==1 & RAC1P==4   ~"Other",
      HISP==1 & RAC1P==5   ~"Other",
      HISP==1 & RAC1P==6   ~"Asian",
      HISP==1 & RAC1P>=7   ~"Other",
      TRUE                 ~"Uncoded"),
    ageprc=case_when(
      AGEP<18              ~"Under 18",
      AGEP>=18 & AGEP<25   ~"18-24",
      AGEP>=25 & AGEP<35   ~"25-34",
      AGEP>=35 & AGEP<45   ~"35-44",
      AGEP>=45 & AGEP<55   ~"45-54",
      AGEP>=55 & AGEP<65   ~"55-64",
      AGEP>=65 & AGEP<75   ~"65-74",
      AGEP>=75 & AGEP<85   ~"75-84",
      AGEP>=85             ~"85 plus",
    )) %>%
  select(SERIALNO,RELSHIPP,PUMA,PUMA_Name,County_Name,PWGTP,WGTP,racerc,AGEP,ageprc,adjustedinc,HINCP,incomerc,NP,SMARTPHONE)

write.csv(combined, file.path(output,"ACS_PUMS2023_Income_Age_Race_NumPersons.csv"), row.names = FALSE, quote = T)

