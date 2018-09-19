
# Create ACS 2013 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2013/ss13hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2013/ss13pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013/hcalif13.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013/hcalif13_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2013/pcalif13.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013/hbayarea13.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013/hbayarea13_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2013/pbayarea13.Rdata"

hcalif13 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif13 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea13 <- merge (hcalif13,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea13 <- merge (pcalif13,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

hcalif13_occ <- subset (hcalif13, !is.na(TEN))
row.names(hcalif13_occ) <- NULL

hbayarea13_occ <- subset (hbayarea13, !is.na(TEN))
row.names(hbayarea13_occ) <- NULL

save(hcalif13, file = CAHH_OUTPUT_RDATA)
save(hbayarea13_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pbayarea13, file = CAPER_OUTPUT_RDATA)

save(hbayarea13, file = BAYHH_OUTPUT_RDATA)
save(hbayarea13_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea13, file = BAYPER_OUTPUT_RDATA)
