
# Create ACS 2021 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2021/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2021/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2021/hcalif21.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2021/pcalif21.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2021/hbayarea21.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2021/pbayarea21.Rdata"

hcalif21 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif21 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea21 <- merge (hcalif21,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea21 <- merge (pcalif21,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

save(hcalif21, file = CAHH_OUTPUT_RDATA)
save(pcalif21, file = CAPER_OUTPUT_RDATA)

save(hbayarea21, file = BAYHH_OUTPUT_RDATA)
save(pbayarea21, file = BAYPER_OUTPUT_RDATA)
