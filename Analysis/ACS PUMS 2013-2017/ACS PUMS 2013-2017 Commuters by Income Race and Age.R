# ACS PUMS 2013-2017 Commuters by Income Race and Age.R

# Create a 2013-2017 Bay Area PUMS dataset for Bay Area commuters by income, race, and age

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/Maureen Wetter"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pbayarea1317.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2013-17/hbayarea1317.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea1317 %>%  
  select(SERIALNO,HINCP,ADJINC)

# Extract bike commuters and recode relevant variables, join with household file for income values

commuters <- pbayarea1317 %>%
  filter(JWTR>=1) %>%                                   # Extract only bike commuters
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
  select(SERIALNO,SPORDER,PUMA,PUMA_Name,County_Name,POWPUMA, PWGTP,AGEP,JWTR,HINCP,ADJINC,
         adjustedinc, agerc,incomerc,HISP,RAC1P,racerc,RELP)

write.csv(commuters, "PUMS2013-2017 Commuters.csv", row.names = FALSE, quote = T)