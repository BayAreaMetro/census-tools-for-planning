# Analyze PUMS data for West Oakland, PUMAs 102 and 104

# Import Libraries

library(knitr)
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(RCurl)
library(RJSONIO)
library(reshape2)
library(httr)

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS14/hbayarea14_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS14/pbayarea14.Rdata"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

# Delete redundant variables for merging

pbayarea14$ADJINC <- pbayarea14$PUMA <- pbayarea14$COUNTY <- pbayarea14$County_Name <- pbayarea14$PUMA_Name <- pbayarea14$RT <- pbayarea14$ST <- NULL

# Merge person and household databases by serial number and delete row names field

merged <- merge (pbayarea14,hbayarea14_occ, by="SERIALNO")
row.names(merged) <- NULL

# Adjust income to constant year 2014 dollars

merged$adjustment=merged$ADJINC/1000000
merged$adjustedinc=merged$HINCP*merged$adjustment

# Select important variables

merged1 <- merged %>%
    select (SERIALNO,PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, adjustedinc, WGTP, DIS, HISP, RAC1P, HINCP, ADJINC)

# Recode race/Hispanic variables and use person weight

final <- merged1 %>%
  mutate(White=ifelse((HISP==1 & RAC1P==1),PWGTP,0)) %>%
  mutate(Black=ifelse((HISP==1 & RAC1P==2),PWGTP,0)) %>%
  mutate(Asian=ifelse((HISP==1 & RAC1P==6),PWGTP,0)) %>%
  mutate(AIAN=ifelse((HISP==1 & (RAC1P>=3 & RAC1P<=5)),PWGTP,0)) %>%
  mutate(NHPI=ifelse((HISP==1 & RAC1P==7),PWGTP,0)) %>%
  mutate(Other=ifelse((HISP==1 & RAC1P>=8),PWGTP,0)) %>%
  mutate(Hispanic=ifelse((HISP>1),PWGTP,0)) %>%
  mutate(Total=PWGTP)%>%
  mutate(Disabled=ifelse((DIS==1), "Yes", "No"))
  