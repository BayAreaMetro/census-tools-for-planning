
# Create ACS 2019 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2019/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2019/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2019/hcalif19.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pcalif19.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2019/hbayarea19.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"

hcalif19 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif19 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea19 <- merge (hcalif19,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea19 <- merge (pcalif19,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

save(hcalif19, file = CAHH_OUTPUT_RDATA)
save(pcalif19, file = CAPER_OUTPUT_RDATA)

save(hbayarea19, file = BAYHH_OUTPUT_RDATA)
save(pbayarea19, file = BAYPER_OUTPUT_RDATA)
