# ACS 2017-2021 PUMS LEP for Travel Diary Survey.R
# Analyze ACS 2017-2021 Bay Area PUMS for LEP populations by Chinese, Spanish, and Tagalog spoken at home

suppressMessages(library(tidyverse))

# Locations
USERPROFILE     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_int         <- file.path(USERPROFILE,"Box","Modeling and Surveys","Surveys","Travel Diary Survey")
BOX_Lanugage    <- file.path(BOX_int,"Biennial Travel Diary Survey","2023 Survey Instrument Development","Language Summaries")


Chinese    = c(1970,2000,2050) # Chinese, Mandarin, and Cantonese, respectively
Spanish    = 1200
Tagalog    = 2920
Vietnamese = 1960

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"
load (PERSON_RDATA)
   
# Recode variables for Chinese, Spanish, and Tagalog

pums <- pbayarea1721 %>% 
  mutate(English_Ability=if_else(ENG %in% c(2,3,4),"LEP","Very Well or Underage")) %>%        # Speaks English less than very well
  mutate(Language=case_when(
           LANP=="1970"    ~"Chinese",
           LANP=="2000"    ~"Chinese",
           LANP=="2050"    ~"Chinese",
           LANP=="1200"    ~"Spanish",
           LANP=="2920"    ~"Tagalog",
           LANP=="1960"    ~"Vietnamese",
           TRUE            ~"All_Others")) %>% 
  select(County_Name,PWGTP,Language,English_Ability)

final <- pums %>% 
  group_by(County_Name,Language,English_Ability) %>% 
  summarize(total=sum(PWGTP)) %>% 
  pivot_wider(.,names_from=Language,values_from=total) %>% 
  ungroup() %>% 
  relocate(All_Others,.after = Vietnamese)

write.csv(final, file.path(BOX_Lanugage,"PUMS 2017-2021 Bay Area Limited-English Speakers.csv"), row.names = FALSE, quote = T)





