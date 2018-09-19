
# Create ACS 2016 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2016/ss16hca.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2016/ss16pca.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2016/hcalif16.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2016/hcalif16_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pcalif16.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"

hcalif16 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif16 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea16 <- merge (hcalif16,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea16 <- merge (pcalif16,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

hcalif16_occ <- subset (hcalif16, !is.na(TEN))
row.names(hcalif16_occ) <- NULL

hbayarea16_occ <- subset (hbayarea16, !is.na(TEN))
row.names(hbayarea16_occ) <- NULL

save(hcalif16, file = CAHH_OUTPUT_RDATA)
save(hcalif16_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pcalif16, file = CAPER_OUTPUT_RDATA)

save(hbayarea16, file = BAYHH_OUTPUT_RDATA)
save(hbayarea16_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea16, file = BAYPER_OUTPUT_RDATA)
