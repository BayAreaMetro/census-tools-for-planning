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

merged <- merge (pbayarea14,hbayarea14_occ, by="SERIALNO")

oakland <- subset (merged, PUMA==102)

merged <- arrange(merged,PUMA.x)
  

