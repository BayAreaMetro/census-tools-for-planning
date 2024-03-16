
# Create ACS 2018 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2018/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2018/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2010.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2018/hcalif18.Rdata"
CAHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2018/hcalif18_occ.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pcalif18.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2018/hbayarea18.Rdata"
BAYHH_OCC_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2018/hbayarea18_occ.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pbayarea18.Rdata"

hcalif18 <- read.csv(CA_HOUSEHOLDS, header=TRUE)
pcalif18 <- read.csv(CA_PERSONS, header=TRUE)
equivalence <- read.csv(EQUIVALENCE, header=TRUE)

hbayarea18 <- merge (hcalif18,equivalence, by.x="PUMA", by.y="PUMARC")
pbayarea18 <- merge (pcalif18,equivalence, by.x="PUMA", by.y="PUMARC")

# Select only occupied households to save to a separate occupied file

save(hcalif18, file = CAHH_OUTPUT_RDATA)
save(pcalif18, file = CAPER_OUTPUT_RDATA)

save(hbayarea18, file = BAYHH_OUTPUT_RDATA)
save(pbayarea18, file = BAYPER_OUTPUT_RDATA)
