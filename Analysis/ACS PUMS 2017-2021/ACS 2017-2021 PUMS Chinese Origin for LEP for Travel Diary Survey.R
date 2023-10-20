# ACS 2017-2021 PUMS Chinese Origin for LEP for Travel Diary Survey.R
# Analyze ACS 2017-2021 Bay Area PUMS for LEP populations by Chinese, Spanish, and Tagalog spoken at home

suppressMessages(library(tidyverse))

# Locations
USERPROFILE     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_int         <- file.path(USERPROFILE,"Box","Modeling and Surveys","Surveys","Travel Diary Survey")
BOX_Language    <- file.path(BOX_int,"Biennial Travel Diary Survey","2023 Survey Instrument Development","Language Summaries")


Chinese    = c(1970,2000,2050) # Chinese, Mandarin, and Cantonese, respectively

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"
load (PERSON_RDATA)
   
# Recode variables for country of birth

pums <- pbayarea1721 %>% 
  filter(LANP %in% Chinese) %>% 
  mutate(English_Ability=if_else(ENG %in% c(2,3,4),"LEP","Very Well or Underage")) %>%        # Speaks English less than very well
  mutate(  Origin=case_when(
           POBP=="207"      ~"China",
           POBP=="209"      ~"HongKong_or_Taiwan",
           POBP=="240"      ~"HongKong_or_Taiwan",
           TRUE             ~"Other_Countries"
         )) %>% 
  select(County_Name,PWGTP,English_Ability,Origin,POBP)

final <- pums %>% 
  group_by(County_Name,English_Ability,Origin) %>% 
  summarize(total=sum(PWGTP)) %>% 
  pivot_wider(.,names_from=Origin,values_from=total) %>% 
  ungroup() %>% 
  relocate(Other_Countries,.after = HongKong_or_Taiwan) %>% 
  filter(English_Ability=="LEP") %>% 
  select(-English_Ability)

# Country of birth other than China,Hong Kong, or Taiwan

birth <- pums %>% 
  filter(Origin=="Other_Countries" & English_Ability=="LEP") 

sort(table(birth$POBP),decreasing = T)

write.csv(final, file.path(BOX_Language,"PUMS 2017-2021 Bay Area Limited-English Chinese Speakers Birthplace.csv"), row.names = FALSE, quote = T)





