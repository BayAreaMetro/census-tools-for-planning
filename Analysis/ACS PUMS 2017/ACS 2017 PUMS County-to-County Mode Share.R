# ACS 2017 PUMS County-to-County Mode Share.R

# Analyze 2017 Bay Area PUMS data for intra-regional Bay Area workers

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/Erin Baldassari/County-to-County Commute/"  # work directory
setwd(wd)

# baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pbayarea17.Rdata"

load (PERSON_RDATA)

# Recode na values for mode (allowing for a commuter-only file), and identify workers who live and work in Bay Area

commuters <- pbayarea17 %>%
  filter(!is.na(JWTR)) %>%                                   # Extract only commuters (folks with a commute mode)
  mutate(
    drive_alone=     if_else(JWTR==1 & JWRIP==1,PWGTP,0L),
    carpool2=        if_else(JWTR==1 & JWRIP==2,PWGTP,0L),
    carpool3p=       if_else(JWTR==1 & JWRIP>=3,PWGTP,0L),
    transit=         if_else(JWTR %in% 2:6,PWGTP,0L),       # Apply person weights for transit and total commuters
    walk=            if_else(JWTR==10,PWGTP,0L),
    bike=            if_else(JWTR==9,PWGTP,0L),
    work_home=       if_else(JWTR==11,PWGTP,0L),
    other=           if_else(JWTR %in% c(7,8,12),PWGTP,0L),
    total_commuters= PWGTP,
    ocounty=         County_Name,
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
      POWPUMA==9700 ~ "Sonoma",
      TRUE          ~ "Outside Bay Area"
          )) %>%
      select(PUMA,ocounty,dcounty,PWGTP,drive_alone,carpool2,carpool3p,transit,walk,bike,work_home,other,total_commuters,JWTR,JWRIP)

# Summarize data and output

summary <- commuters %>%
  group_by(ocounty,dcounty) %>%
  summarize(drive_alone=sum(drive_alone),carpool2=sum(carpool2),carpool3p=sum(carpool3p),transit=sum(transit),walk=sum(walk),
            bike=sum(bike),work_home=sum(work_home),other=sum(other),total_commuters=sum(total_commuters)) 
  
write.csv(summary, "PUMS2017 County-to-County Commuters.csv", row.names = FALSE, quote = T)


