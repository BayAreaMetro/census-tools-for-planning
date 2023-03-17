# ACS PUMS 2019 People by Income.R

suppressMessages(library(tidyverse))
output <- "M:/Data/Requests/Alix Bockelman"  # work directory

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"
HH_RDATA     = "M:/Data/Census/PUMS/PUMS 2019/hbayarea19.Rdata"

load (PERSON_RDATA)
load(HH_RDATA) 

household <- hbayarea19 %>%  
  select(SERIALNO,HINCP,ADJINC)

# Extract bike commuters and recode relevant variables, join with household file for income values

final <- pbayarea19 %>%
  filter(RELSHIPP<=36) %>% 
  select(-ADJINC) %>%                                   # Remove this variable and use joined version
  left_join(.,household,by="SERIALNO") %>% 
  mutate(
    adjustedinc=HINCP*(ADJINC/1000000),
    incomerc=case_when(
      adjustedinc <25000                         ~"1_less to 25k",
      adjustedinc >=25000 & adjustedinc <50000   ~"2_25k to 49,999",
      adjustedinc >=50000 & adjustedinc <75000   ~"3_50k to 74,999",
      adjustedinc >=75000 & adjustedinc <100000  ~"4_75k to 99,999",
      adjustedinc >= 100000                      ~"5_100k+",
      TRUE                                       ~"Uncoded, group quarters")) %>% 
  group_by(incomerc) %>% 
  summarize(total=sum(PWGTP)) %>% 
  mutate(share=total/sum(total)) %>% 
  ungroup()

   

write.csv(final, "PUMS2019 Household Persons by Income.csv", row.names = FALSE, quote = T)
