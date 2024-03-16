
# Create ACS 2022 PUMS files in r format

# Import Libraries

suppressMessages(library(dplyr))
suppressMessages(library(feather))

# Set up directories with csv files for PUMS data

CA_HOUSEHOLDS = "M:/Data/Census/PUMS/PUMS 2022/psam_h06.csv"
CA_PERSONS = "M:/Data/Census/PUMS/PUMS 2022/psam_p06.csv"
EQUIVALENCE = "m:/data/census/corrlib/Bay_puma_2020.csv"

CAHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2022/hcalif22.Rdata"
CAPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2022/pcalif22.Rdata"

BAYHH_OUTPUT_RDATA   = "M:/Data/Census/PUMS/PUMS 2022/hbayarea22.Rdata"
BAYPER_OUTPUT_RDATA = "M:/Data/Census/PUMS/PUMS 2022/pbayarea22.Rdata"

BAYHH_OUTPUT_CSV = str_replace(BAYHH_OUTPUT_RDATA, ".Rdata", ".csv")
BAYPER_OUTPUT_CSV = str_replace(BAYPER_OUTPUT_RDATA, ".Rdata", ".csv")

hcalif22 <- read.csv(CA_HOUSEHOLDS, colClasses = c(PUMA = "character",ST = "character"))
pcalif22 <- read.csv(CA_PERSONS, colClasses = c(PUMA = "character",ST = "character"))
equivalence <- read.csv(EQUIVALENCE, colClasses = c(PUMARC = "character",State = "character"))

hbayarea22 <- merge (hcalif22,equivalence, by.x=c("ST","PUMA"), by.y=c("State","PUMARC"))
pbayarea22 <- merge (pcalif22,equivalence, by.x=c("ST","PUMA"), by.y=c("State","PUMARC"))

# Save out files in R format

save(hcalif22, file = CAHH_OUTPUT_RDATA)
save(pcalif22, file = CAPER_OUTPUT_RDATA)

save(hbayarea22, file = BAYHH_OUTPUT_RDATA)
save(pbayarea22, file = BAYPER_OUTPUT_RDATA)

write.csv(hbayarea22, file = BAYHH_OUTPUT_CSV, row.names=FALSE)
write.csv(pbayarea22, file = BAYPER_OUTPUT_CSV, row.names=FALSE)

# feather

BAYHH_OUTPUT_FEATHER = str_replace(BAYHH_OUTPUT_RDATA, ".Rdata", ".feather")
BAYPER_OUTPUT_FEATHER = str_replace(BAYPER_OUTPUT_RDATA, ".Rdata", ".feather")

write_feather(hbayarea22, BAYHH_OUTPUT_FEATHER)
write_feather(pbayarea22, BAYPER_OUTPUT_FEATHER)
