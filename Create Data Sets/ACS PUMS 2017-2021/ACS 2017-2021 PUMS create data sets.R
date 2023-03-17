
# Create ACS 2017-2021 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2017-21/csv_hca/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2017-21/csv_pca/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017-21/hcalif1721.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pcalif1721.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2017-21/hbayarea1721.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"

hcalif1721 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1721 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1721 <- merge (hcalif1721,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea1721 <- merge (pcalif1721,equivalence, by.x="PUMA", by.y="PUMARC")

# Save out files

save(hcalif1721, file = CAHH_OUTPUT_RDATA)
save(pcalif1721, file = CAPER_OUTPUT_RDATA)

save(hbayarea1721, file = BAYHH_OUTPUT_RDATA)
save(pbayarea1721, file = BAYPER_OUTPUT_RDATA)
