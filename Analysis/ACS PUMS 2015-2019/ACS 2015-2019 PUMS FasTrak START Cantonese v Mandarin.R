# ACS 2015-2019 PUMS FasTrak START Cantonese v Mandarin.R
# Analyze 2015-2019 Bay Area PUMS for people living along the I-880 corridor and speak Cantonese vs. Mandarin at home
# Speakers who are: LEP, <200% poverty, in relevant PUMAs, and drive/carpool to work
# For the generic "Chinese" speakers, we could just have that total number available, but wouldn't use it for comparison
# https://www2.census.gov/geo/maps/dc10map/PUMA_RefMap/st06_ca/puma0608104/DC10PUMA0608104_000.pdf
# 00102-109,08502-08505,08509-08510,08513

suppressMessages(library(dplyr))
library(reldist)    # For weighted median calculation
wd <- "M:/Data/Requests/Ky-Nam Miller"  # work directory
setwd(wd)

pumas = c(102,103,104,105,106,107,108,109,8502,8503,8504,8505,8509,8510,8513) # PUMAs of interest for this study
languages = c(1970,2000,2050) # Chinese, Mandarin, and Cantonese, respectively

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2015-19/pbayarea1519.Rdata"
load (PERSON_RDATA)

# Recode variables for Mandarin, Cantonese, and Chinese and select people based on above-described criteria

pums <- pbayarea1519 %>% 
  filter((PUMA %in% pumas) &          # PUMAs of interest
           (ENG %in% c(2,3,4)) &      # Speaks English less than very well
           (LANP %in% languages) &    # Languages spoken at home are Chinese, Mandarin, and Cantonese
           (POVPIP < 200) &           # Poverty less than 200 percent
           (JWTRNS == 1)) %>%         # Takes a car to work
  mutate(Dialect=recode(LANP,"1970"="3_Chinese","2000"="2_Mandarin","2050"="1_Cantonese"))

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


