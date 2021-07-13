# ACS 2019 County of Work Transit Mode Share.R

# Analyze 2019 Bay Area PUMS data for intra-regional Bay Area workers

suppressMessages(library(tidyverse))
wd <- "H:/Presentations/Planning Section 2021/"  # work directory
setwd(wd)

baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"

load (PERSON_RDATA)

# Recode na values for mode (allowing for a commuter-only file), and identify workers who live and work in Bay Area

commuters <- pbayarea19 %>%
  filter(!is.na(JWTRNS)) %>%                                   # Extract only commuters (folks with a commute mode)
  mutate(
    transit_commuters=if_else(JWTRNS %in% 2:6,PWGTP,0L),       # Apply person weights for transit and total commuters
    work_home=if_else(JWTRNS==11,PWGTP,0L),
    total_commuters=PWGTP-work_home) %>%
  filter(POWPUMA %in% baypowpuma & POWSP==6) %>%                                # Remove records with a destination outside Bay Area
  mutate(POWNAME=recode(POWPUMA,"100"="Alameda",
                        "1300"="Contra Costa",
                        "4100"="Marin",
                        "5500"="Napa",
                        "7500"="San Francisco",
                        "8100"="San Mateo",
                        "8500"="Santa Clara",
                        "9500"="Solano",
                        "9700"="Sonoma"
                      )) %>% 
  select(PUMA,County_Name, PWGTP,JWTRNS,transit_commuters,work_home,total_commuters,POWPUMA,POWSP,POWNAME)

# Summarize transit and total commuters and calculate share of transit commuters, output data

summary <- commuters %>%
  group_by(POWNAME) %>%
  summarize(transit_commuters=sum(transit_commuters),total_commuters=sum(total_commuters)) %>%
  mutate(share_transit=transit_commuters/total_commuters)

summary_alameda <- commuters %>%
  group_by(County_Name, POWNAME) %>%
  summarize(transit_commuters=sum(transit_commuters),total_commuters=sum(total_commuters)) %>%
  mutate(share_transit=transit_commuters/total_commuters)
  
write.csv(summary, "PUMS2019 Transit Commuter Share by County of Work.csv", row.names = FALSE, quote = T)


