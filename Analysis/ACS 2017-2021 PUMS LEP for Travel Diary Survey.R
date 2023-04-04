# ACS 2017-2021 PUMS LEP for Travel Diary Survey.R
# Analyze ACS 2017-2021 Bay Area PUMS for LEP populations by Chinese, Spanish, and Tagalog spoken at home

suppressMessages(library(dplyr))

# Locations
USERPROFILE     <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
BOX_int         <- file.path(USERPROFILE,"Box","Modeling and Surveys","Surveys","Travel Diary Survey")
BOX_Lanugage    <- file.path(BOX_int,"Biennial Travel Diary Survey","2023 Survey Instrument Development","Language Summaries")


Chinese    = c(1970,2000,2050) # Chinese, Mandarin, and Cantonese, respectively
Tagalog    = 2920
Vietnamese = 1960

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"
load (PERSON_RDATA)
   
# Recode variables for Chinese, Spanish, and Tagalog

pums <- pbayarea1721 %>% 
  mutate(LEP=if_else(ENG %in% c(2,3,4),1,0)) %>%        # Speaks English less than very well
  filter (LANP %in% c(Chinese,Tagalog,Vietnamese))      # Languages spoken at home are Chinese, Mandarin, and Cantonese


# Summarize by language then do median age by dialect

final <- pums %>% 
  group_by(Dialect) %>% 
  summarize(Total=sum(PWGTP))

median_age <- pbayarea1519 %>% 
  filter((LANP %in% languages) & (PUMA %in% pumas)) %>% 
  mutate(Dialect=recode(LANP,"1970"="3_Chinese","2000"="2_Mandarin","2050"="1_Cantonese")) %>% 
  group_by(Dialect) %>% 
  summarize(med_age=wtd.quantile(AGEP, q=0.5, na.rm = FALSE, weight=PWGTP))

write.csv(final, "PUMS2015-2019 Chinese LEP Commuters.csv", row.names = FALSE, quote = T)
write.csv(median_age, "PUMS2015-2019 Chinese Median Age.csv", row.names = FALSE, quote = T)


