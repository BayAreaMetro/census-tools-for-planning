# ACS 2012-2016 PUMS In-Commute Analysis.R

# Analyze 2012-2016 Bay Area PUMS data for intra-regional Bay Area workers

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/Lisa Zorn/County-to-County Commute/"  # work directory
setwd(wd)

# baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pbayarea1216.Rdata"

load (PERSON_RDATA)

# Recode na values for mode (allowing for a commuter-only file), and identify workers who live and work in Bay Area

commuters <- pbayarea1216 %>%
  filter(!is.na(JWTR)) %>%                                   # Extract only commuters (folks with a commute mode)
  mutate(
    transit_commuters=if_else(JWTR %in% 2:6,PWGTP,0L),       # Apply person weights for transit and total commuters
    total_commuters=PWGTP,
    ocounty=County_Name,
    dcounty=
    case_when(
      POWPUMA==100 ~  "Alameda",                             # Recode POWPUMA codes to county names
      POWPUMA==1300 ~ "Contra Costa",
      POWPUMA==4100 ~ "Marin",
      POWPUMA==5500 ~ "Napa",
      POWPUMA==7500 ~ "San Francisco",
      POWPUMA==8100 ~ "San Mateo",
      POWPUMA==8500 ~ "Santa Clara",
      POWPUMA==9500 ~ "Solano",
      POWPUMA==9700 ~ "Sonoma"
          )) %>%
  filter(!is.na(dcounty)) %>%                                # Remove records with a destination outside Bay Area
      select(PUMA,ocounty,dcounty,PWGTP,JWTR,transit_commuters,total_commuters)

# Summarize transit and total commuters and calculate share of transit commuters, output data

summary <- commuters %>%
  group_by(ocounty,dcounty) %>%
  summarize(transit_commuters=sum(transit_commuters),total_commuters=sum(total_commuters)) %>%
  mutate(share_transit=transit_commuters/total_commuters)
  
write.csv(summary, "PUMS2012-2016 County-to-County Commuters.csv", row.names = FALSE, quote = T)


