# ACS-2022-Part-Time-Full-Time-Work-at-Home.R

# Analyze 2022 Bay Area PUMS data for part-time and full-time work at home shares

suppressMessages(library(tidyverse))

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2022/pbayarea22.Rdata"

load (PERSON_RDATA)

# Filter for commuters and create part-time and full-time workers
# Definitions of full- and part-time workers come from PopulationSim script:
# https://github.com/BayAreaMetro/populationsim/blob/2956cd0afffbe0a3b74cf3254c644bbde444f829/bay_area/create_seed_population.py#L213
# Full-time workers is more than half the year and 35+ hours a week; part-time workers is all others

commuters <- pbayarea22 %>%
  filter(!is.na(JWTRNS)) %>%                                   # Extract only commuters (folks with a commute mode)
  mutate(employ_status=if_else((WKWN>=27 & WKHP>=35),"full-time","part-time"),
         work_home=if_else(JWTRNS==11,PWGTP,0L),
         total_workers=PWGTP)

summation <- commuters %>% 
  group_by(employ_status) %>% 
  summarize(total_work_home=sum(work_home),total_workers=sum(total_workers)) %>% 
  mutate(share=total_work_home/total_workers)

print(summation)
