# ACS 2017 PUMS Age Distribution of Chinese Speakers.R
# Analyze 2017 Bay Area PUMS data for age distribution of Chinese speakers
# December 11, 2019

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/Lysa Hale/Chinese Speakers by Age/"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pbayarea17.Rdata"
load (PERSON_RDATA)

# Filter only people speaking one Lysa's selected language codes for Chinese and recode age

chinese <- pbayarea17 %>%
  filter(LANP %in% c(1970,2000,2030,2050)) %>% mutate(  
  agerc=case_when(
    AGEP<20              ~"1_less_than_20",
    AGEP>=20 & AGEP<30   ~"2_between 20 and 30",
    AGEP>=30 & AGEP<40   ~"3_between 30 and 40",
    AGEP>=40 & AGEP<50   ~"4_between 40 and 50",
    AGEP>=50 & AGEP<60   ~"5_between 50 and 60",
    AGEP>=60 & AGEP<70   ~"6_between 60 and 70",
    AGEP>=70             ~"7_70+",
    TRUE                 ~"Uncoded"))

# Summarize data and output

summary <- chinese %>%
  group_by(County_Name, agerc) %>%
  summarize(total=sum(PWGTP)) %>% 
   spread (., agerc, total, fill=0)
  
write.csv(summary, "PUMS2017 Chinese Speakers by Age.csv", row.names = FALSE, quote = T)


