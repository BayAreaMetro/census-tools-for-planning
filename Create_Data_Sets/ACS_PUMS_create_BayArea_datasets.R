
# Generic script to create ACS Bay Area dataset
USAGE = "
  Reads downloaded raw Californa ACS PUMS data, joins with .
"

# Import Libraries
suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(feather)
    library(argparser)
})

argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="survey",  help="Either acs1 or acs5")
argparser <- add_argument(parser=argparser, arg="year",    help="Survey year", type="numeric")
# parse the command line arguments
argv <- parse_args(argparser)
stopifnot(argv$survey %in% c("acs1","acs5"))
stopifnot(argv$year >= 2020)

# Set up directories with csv files for PUMS data
PUMS_ROOT_DIR <- "M:/Data/Census/PUMS"
if (argv$survey == "acs5") {
    PUMS_DIR <- sprintf("PUMS %d-%02d", argv$year-4, argv$year %% 100)
} else {
    PUMS_DIR <- sprintf("PUMS %d", argv$year)
}
print(paste("PUMS_DIR:", PUMS_DIR))

CA_HOUSEHOLDS = file.path(PUMS_ROOT_DIR, PUMS_DIR, "psam_h06.csv")
CA_PERSONS    = file.path(PUMS_ROOT_DIR, PUMS_DIR, "psam_p06.csv")
EQUIVALENCE   = "M:/Data/Census/corrlib/Bay_puma_2020.csv"

CAHH_OUTPUT_RDATA  = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("hcalif%02d.Rdata", argv$year %% 100))
CAPER_OUTPUT_RDATA = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("pcalif%02d.Rdata", argv$year %% 100))

BAYHH_OUTPUT_RDATA   = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("hbayarea%02d.Rdata", argv$year %% 100))
BAYPER_OUTPUT_RDATA  = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("pbayarea%02d.Rdata", argv$year %% 100))

BAYHH_OUTPUT_CSV  = str_replace(BAYHH_OUTPUT_RDATA, ".Rdata", ".csv")
BAYPER_OUTPUT_CSV = str_replace(BAYPER_OUTPUT_RDATA, ".Rdata", ".csv")

PUMA_VAR  <- "PUMA"
if ((argv$survey == "acs5") && (argv$year <= 2022)) {
    # there's a PUMA10 and PUMA20
    PUMA_VAR  <- "PUMA20"
}

STATE_VAR <- "ST"
# state variable was renamed for 2023
if (argv$year >= 2023) {
    STATE_VAR <- "STATE"
}
PUMA_STATE_COLCLASSES <- c("character","character")
names(PUMA_STATE_COLCLASSES) <- c(PUMA_VAR, STATE_VAR)
print("PUMA_STATE_COLCLASSES:")
print(PUMA_STATE_COLCLASSES)

hcalif <- read.csv(CA_HOUSEHOLDS,  colClasses = PUMA_STATE_COLCLASSES)
pcalif <- read.csv(CA_PERSONS,     colClasses = PUMA_STATE_COLCLASSES)
print(paste("Read",nrow(hcalif),"rows from",CA_HOUSEHOLDS))
print(paste("Read",nrow(pcalif),"rows from",CA_PERSONS))

str(hcalif)
str(pcalif)

# columns are State, County_Name, PUMARC, PUMA_Name
#   State County_Name PUMARC                                                                      PUMA_Name
# 1    06     Alameda  00101                               Alameda County (North)--Berkeley & Albany Cities
# 2    06     Alameda  00111 Alameda County (Northwest)--Oakland City (Downtown/West Oakland/North Oakland)
# 3    06     Alameda  00112                       Alameda County (Northwest)--Oakland City (Oakland Hills)
# 4    06     Alameda  00113            Alameda County (West)--Oakland City (Elmhurst/Central/East Oakland)
# 5    06     Alameda  00114      Alameda County (West)--San Leandro, Alameda, Emeryville & Piedmont Cities
# 6    06     Alameda  00115                            Alameda County (Northwest)--Castro Valley & Ashland
equivalence <- read.csv(EQUIVALENCE, colClasses = c(PUMARC = "character",State = "character"))
print(paste("Read",nrow(equivalence),"rows from",EQUIVALENCE))

# Save out files in R format

save(hcalif, file = CAHH_OUTPUT_RDATA)
print(paste("Saved",CAHH_OUTPUT_RDATA))
save(pcalif, file = CAPER_OUTPUT_RDATA)
print(paste("Saved",CAPER_OUTPUT_RDATA))

hbayarea <- merge (hcalif,equivalence, by.x=c(STATE_VAR,PUMA_VAR), by.y=c("State","PUMARC"))
pbayarea <- merge (pcalif,equivalence, by.x=c(STATE_VAR,PUMA_VAR), by.y=c("State","PUMARC"))

save(hbayarea, file = BAYHH_OUTPUT_RDATA)
print(paste("Saved",BAYHH_OUTPUT_RDATA))
save(pbayarea, file = BAYPER_OUTPUT_RDATA)
print(paste("Saved",BAYPER_OUTPUT_RDATA))


write.csv(hbayarea, file = BAYHH_OUTPUT_CSV, row.names=FALSE)
print(paste("Wrote",BAYHH_OUTPUT_CSV))

write.csv(pbayarea, file = BAYPER_OUTPUT_CSV, row.names=FALSE)
print(paste("Wrote",BAYPER_OUTPUT_CSV))

# feather

BAYHH_OUTPUT_FEATHER  = str_replace(BAYHH_OUTPUT_RDATA, ".Rdata", ".feather")
BAYPER_OUTPUT_FEATHER = str_replace(BAYPER_OUTPUT_RDATA, ".Rdata", ".feather")

write_feather(hbayarea, BAYHH_OUTPUT_FEATHER)
print(paste("Wrote",BAYHH_OUTPUT_FEATHER))

write_feather(pbayarea, BAYPER_OUTPUT_FEATHER)
print(paste("Wrote",BAYPER_OUTPUT_FEATHER))
