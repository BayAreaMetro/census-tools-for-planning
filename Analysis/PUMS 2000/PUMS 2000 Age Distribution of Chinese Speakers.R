# PUMS 2000 Age Distribution of Chinese Speakers.R
# Analyze 2000 Bay Area PUMS data for age distribution of Chinese speakers
# December 11, 2019

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/Lysa Hale/Chinese Speakers by Age/"  # work directory
setwd(wd)

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2000/pbayarea00.Rdata"
load (PERSON_RDATA)

# Filter only people speaking one Lysa's selected language codes for Chinese and recode age
"
708 Chinese
709 Hakka
710 Kan, Hsiang
711 Cantonese
712 Mandarin
713 Fuchow
714 Formosa
715 Wu
"


chinese <- pbayarea00 %>%
  filter(lang1 %in% c(708,709,710,711,712,713,714,715)) %>% mutate(  
  agerc=case_when(
    age<20              ~"1_less_than_20",
    age>=20 & age<30    ~"2_between 20 and 30",
    age>=30 & age<40    ~"3_between 30 and 40",
    age>=40 & age<50    ~"4_between 40 and 50",
    age>=50 & age<60    ~"5_between 50 and 60",
    age>=60 & age<70    ~"6_between 60 and 70",
    age>=70             ~"7_70+",
    TRUE                 ~"Uncoded"))

# Summarize data and output

summary <- chinese %>%
  group_by(COUNTY, agerc) %>%
  summarize(total=sum(pweight)) %>% 
   spread (., agerc, total, fill=0)
  
write.csv(summary, "PUMS2000 Chinese Speakers by Age.csv", row.names = FALSE, quote = T)


