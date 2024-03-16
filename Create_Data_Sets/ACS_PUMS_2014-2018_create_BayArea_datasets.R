
# Create ACS 2014-2018 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2014-18/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2014-18/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2014-18/hcalif1418.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2014-18/pcalif1418.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2014-18/hbayarea1418.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2014-18/pbayarea1418.Rdata"

hcalif1418 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif1418 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea1418 <- merge (hcalif1418,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea1418 <- merge (pcalif1418,equivalence, by.x="PUMA", by.y="PUMARC")

# Save out r versions of the files

save(hcalif1418, file = CAHH_OUTPUT_RDATA)
save(pcalif1418, file = CAPER_OUTPUT_RDATA)

save(hbayarea1418, file = BAYHH_OUTPUT_RDATA)
save(pbayarea1418, file = BAYPER_OUTPUT_RDATA)





