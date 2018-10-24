
# Create ACS 2012-2016 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2012-16/ss16hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2012-16/ss16pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2012-16/hcalif1216.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2012-16/hcalif1216_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pcalif1216.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pbayarea1216.Rdata"

hcalif1216 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1216 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1216 <- merge (hcalif1216,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea1216 <- merge (pcalif1216,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

hcalif1216_occ <- subset (hcalif1216, !is.na(TEN))
row.names(hcalif1216_occ) <- NULL

hbayarea1216_occ <- subset (hbayarea1216, !is.na(TEN))
row.names(hbayarea1216_occ) <- NULL

save(hcalif1216, file = CAHH_OUTPUT_RDATA)
save(hcalif1216_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pcalif1216, file = CAPER_OUTPUT_RDATA)

save(hbayarea1216, file = BAYHH_OUTPUT_RDATA)
save(hbayarea1216_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea1216, file = BAYPER_OUTPUT_RDATA)
