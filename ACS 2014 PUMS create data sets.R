
# Create ACS 2014 PUMS files in r format

# Import Libraries

library(knitr)
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(RCurl)
library(RJSONIO)
library(reshape2)
library(httr)

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS14/csv_hca/ss14hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS14/csv_pca/ss14pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS14/hcalif14.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS14/hcalif14_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS14/pcalif14.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS14/hbayarea14.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS14/hbayarea14_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS14/pbayarea14.Rdata"

hcalif14 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif14 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea14 <- merge (hcalif14,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea14 <- merge (pcalif14,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

hcalif14_occ <- subset (hcalif14, !is.na(TEN))
row.names(hcalif14_occ) <- NULL

hbayarea14_occ <- subset (hbayarea14, !is.na(TEN))
row.names(hbayarea14_occ) <- NULL

save(hcalif14, file = CAHH_OUTPUT_RDATA)
save(hbayarea14_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pbayarea14, file = CAPER_OUTPUT_RDATA)

save(hbayarea14, file = BAYHH_OUTPUT_RDATA)
save(hbayarea14_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea14, file = BAYPER_OUTPUT_RDATA)
