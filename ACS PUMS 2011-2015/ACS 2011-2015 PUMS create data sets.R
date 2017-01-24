# ACS 2011-2015 PUMS create data sets.R
# Create ACS 2014 PUMS files in r format
# SI
# January 19, 2017

# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS1115/ss15hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS1115/ss15pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma5_cens2k_ACS1012.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS1115/hcalif1115.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS1115/hcalif1115_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS1115/pcalif1115.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS1115/hbayarea1115.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS1115/hbayarea1115_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS1115/pbayarea1115.Rdata"

hcalif1115 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1115 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1115 <- merge (hcalif1115,equivalence, by=c("PUMA00","PUMA10"))
pbayarea1115 <- merge (pcalif1115,equivalence, by=c("PUMA00","PUMA10"))

# Select only occupied households (no vacancies or group quarters) to save to a separate occupied file

hcalif1115_occ <- subset (hcalif1115, !is.na(TEN))

hbayarea1115_occ <- subset (hbayarea1115, !is.na(TEN))

save(hcalif1115, file = CAHH_OUTPUT_RDATA)
save(hbayarea1115_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pbayarea1115, file = CAPER_OUTPUT_RDATA)

save(hbayarea1115, file = BAYHH_OUTPUT_RDATA)
save(hbayarea1115_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea1115, file = BAYPER_OUTPUT_RDATA)


