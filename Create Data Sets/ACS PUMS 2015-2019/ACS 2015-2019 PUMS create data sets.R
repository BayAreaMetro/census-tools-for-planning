
# Create ACS 2015-2019 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2015-19/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2015-19/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2015-19/hcalif1519.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2015-19/pcalif1519.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2015-19/hbayarea1519.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2015-19/pbayarea1519.Rdata"

hcalif1519 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1519 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1519 <- merge (hcalif1519,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea1519 <- merge (pcalif1519,equivalence, by.x="PUMA", by.y="PUMARC")

# Save out r versions of the files

save(hcalif1519, file = CAHH_OUTPUT_RDATA)
save(pcalif1519, file = CAPER_OUTPUT_RDATA)

save(hbayarea1519, file = BAYHH_OUTPUT_RDATA)
save(pbayarea1519, file = BAYPER_OUTPUT_RDATA)





