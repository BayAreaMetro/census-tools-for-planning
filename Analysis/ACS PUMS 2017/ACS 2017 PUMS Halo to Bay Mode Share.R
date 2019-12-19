# ACS 2017 PUMS Halo to Bay Mode Share.R

# Analyze 2017 Halo to Bay data for workers

suppressMessages(library(dplyr))
wd <- "M:/Data/Requests/John Goodwin/Halo Commutes/"  # work directory
setwd(wd)

# baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files, mark location of equivalence file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pcalif17.Rdata"
equivalence = "M:/Data/Census/corrlib/CA_2010_PUMA_County.csv"
equivalence <- read.csv(equivalence,header = TRUE)


load (PERSON_RDATA)

pbayarea17 <- pcalif17 %>% 
  left_join(.,equivalence, by="PUMA") %>%
  filter(POWPUMA %in% c(100,1300,4100,5500,7500,8100,8500,9500,9700)) %>% mutate(
    POWPUMA_Name = case_when(
      POWPUMA==100       ~ "Alameda",
      POWPUMA==1300      ~ "Contra Costa",
      POWPUMA==4100      ~ "Marin",
      POWPUMA==5500      ~ "Napa",
      POWPUMA==7500      ~ "San Francisco",
      POWPUMA==8100      ~ "San Mateo",
      POWPUMA==8500      ~ "Santa Clara",
      POWPUMA==9500      ~ "Solano",
      POWPUMA==9700      ~ "Sonoma",
      TRUE               ~ "Unrecoded"
    )
  )

# Recode na values for mode (allowing for a commuter-only file), and identify workers who live and work in Bay Area

commuters <- pbayarea17 %>%
  mutate(
    drive_alone=     if_else(JWTR==1 & JWRIP==1,PWGTP,0L),
    carpool2=        if_else(JWTR==1 & JWRIP==2,PWGTP,0L),
    carpool3p=       if_else(JWTR==1 & JWRIP>=3,PWGTP,0L),
    transit=         if_else(JWTR %in% 2:6,PWGTP,0L),       # Apply person weights for transit and total commuters
    walk=            if_else(JWTR==10,PWGTP,0L),
    bike=            if_else(JWTR==9,PWGTP,0L),
    work_home=       if_else(JWTR==11,PWGTP,0L),
    other=           if_else(JWTR %in% c(7,8,12),PWGTP,0L),
    total_commuters= PWGTP
          ) %>%
      select(PUMA,PWGTP,drive_alone,carpool2,carpool3p,transit,walk,bike,work_home,other,total_commuters,JWTR,JWRIP,POWPUMA,POWPUMA_Name,Designation)

# Summarize data and output

summary <- commuters %>%
  group_by(Designation,POWPUMA_Name) %>%
  summarize(drive_alone=sum(drive_alone),carpool2=sum(carpool2),carpool3p=sum(carpool3p),transit=sum(transit),walk=sum(walk),
            bike=sum(bike),work_home=sum(work_home),other=sum(other),total_commuters=sum(total_commuters)) 
  
write.csv(summary, "PUMS2017 Halo County-to-County Commuters.csv", row.names = FALSE, quote = T)


