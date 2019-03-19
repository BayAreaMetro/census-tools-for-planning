
# Create ACS 2013-2017 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2013-17/csv_hca/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2013-17/csv_pca/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013-17/hcalif1317.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pcalif1317.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2013-17/hbayarea1317.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pbayarea1317.Rdata"

hcalif1317 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1317 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1317 <- merge (hcalif1317,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea1317 <- merge (pcalif1317,equivalence, by.x="PUMA", by.y="PUMARC")

# Save out r versions of the files

save(hcalif1317, file = CAHH_OUTPUT_RDATA)
save(pcalif1317, file = CAPER_OUTPUT_RDATA)

save(hbayarea1317, file = BAYHH_OUTPUT_RDATA)
save(pbayarea1317, file = BAYPER_OUTPUT_RDATA)





