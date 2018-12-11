
# Create ACS 2017 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2017/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2017/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017/hcalif17.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017/hcalif17_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pcalif17.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017/hbayarea17.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017/hbayarea17_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pbayarea17.Rdata"

hcalif17 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif17 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea17 <- merge (hcalif17,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea17 <- merge (pcalif17,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

hcalif17_occ <- subset (hcalif17, !is.na(TEN))
row.names(hcalif17_occ) <- NULL

hbayarea17_occ <- subset (hbayarea17, !is.na(TEN))
row.names(hbayarea17_occ) <- NULL

save(hcalif17, file = CAHH_OUTPUT_RDATA)
save(hcalif17_occ, file = CAHH_OCC_OUTPUT_RDATA)
save(pcalif17, file = CAPER_OUTPUT_RDATA)

save(hbayarea17, file = BAYHH_OUTPUT_RDATA)
save(hbayarea17_occ, file = BAYHH_OCC_OUTPUT_RDATA)
save(pbayarea17, file = BAYPER_OUTPUT_RDATA)
