# ACS 2010 PUMS create data sets.R
# Create ACS 2010 PUMS files in r format
# SI
# August 29, 2019

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2010/ss10hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2010/ss10pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma5_cens2k_ACS1012.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2010/hcalif10.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2010/pcalif10.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2010/hbayarea10.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2010/pbayarea10.Rdata"

hcalif10 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif10 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea10 <- merge (hcalif10,equivalence, by.x="PUMA", by.y="PUMA00")
pbayarea10 <- merge (pcalif10,equivalence, by.x="PUMA", by.y="PUMA00")

save(hcalif10, file = CAHH_OUTPUT_RDATA)
save(pcalif10, file = CAPER_OUTPUT_RDATA)

save(hbayarea10, file = BAYHH_OUTPUT_RDATA)
save(pbayarea10, file = BAYPER_OUTPUT_RDATA)
