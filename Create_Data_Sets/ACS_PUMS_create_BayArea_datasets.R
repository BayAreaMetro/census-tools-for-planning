
# Generic script to create ACS Bay Area dataset
USAGE = "
  Reads downloaded raw Californa ACS PUMS data, joins with M:/Data/Census/corrlib/Bay_puma_[2010,2020].csv
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
    OUTPUT_YEAR_STR <- sprintf("%02d%02d", (argv$year-4) %% 100, argv$year %% 100)
} else {
    PUMS_DIR <- sprintf("PUMS %d", argv$year)
    OUTPUT_YEAR_STR <- sprintf("%02d", argv$year %% 100)
}
print(paste("PUMS_DIR:", PUMS_DIR))
print(paste("OUTPUT_YEAR_STR:", OUTPUT_YEAR_STR))

CA_HOUSEHOLDS  = file.path(PUMS_ROOT_DIR, PUMS_DIR, "psam_h06.csv")
CA_PERSONS     = file.path(PUMS_ROOT_DIR, PUMS_DIR, "psam_p06.csv")

CAHH_OUTPUT_RDATA  = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("hcalif%s.Rdata", OUTPUT_YEAR_STR))
CAPER_OUTPUT_RDATA = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("pcalif%s.Rdata", OUTPUT_YEAR_STR))

BAYHH_OUTPUT_RDATA   = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("hbayarea%s.Rdata", OUTPUT_YEAR_STR))
BAYPER_OUTPUT_RDATA  = file.path(PUMS_ROOT_DIR, PUMS_DIR, sprintf("pbayarea%s.Rdata", OUTPUT_YEAR_STR))

BAYHH_OUTPUT_CSV  = str_replace(BAYHH_OUTPUT_RDATA, ".Rdata", ".csv")
BAYPER_OUTPUT_CSV = str_replace(BAYPER_OUTPUT_RDATA, ".Rdata", ".csv")

PUMA_STATE_COLCLASSES = c(
    "PUMA"="character",
    "PUMA10"="character",
    "PUMA20"="character",
    "ST"="character",
    "STATE"="character"
)

print("PUMA_STATE_COLCLASSES:")
print(PUMA_STATE_COLCLASSES)

hcalif <- read.csv(CA_HOUSEHOLDS,  colClasses = PUMA_STATE_COLCLASSES)
pcalif <- read.csv(CA_PERSONS,     colClasses = PUMA_STATE_COLCLASSES)
print(paste("Read",nrow(hcalif),"rows from",CA_HOUSEHOLDS))
print(paste("Read",nrow(pcalif),"rows from",CA_PERSONS))

str(hcalif)
str(pcalif)

# Save out files in R format

save(hcalif, file = CAHH_OUTPUT_RDATA)
print(paste("Saved",CAHH_OUTPUT_RDATA))
save(pcalif, file = CAPER_OUTPUT_RDATA)
print(paste("Saved",CAPER_OUTPUT_RDATA))

EQUIVALENCE_10 = "M:/Data/Census/corrlib/Bay_puma_2010.csv"
EQUIVALENCE_20 = "M:/Data/Census/corrlib/Bay_puma_2020.csv"

# columns are COUNTY, County_Name, PUMARC, PUMA_Name
#   COUNTY County_Name PUMARC                                                                      PUMA_Name
# 1  06001     Alameda  00101                               Alameda County (North)--Berkeley & Albany Cities
# 2  06001     Alameda  00111 Alameda County (Northwest)--Oakland City (Downtown/West Oakland/North Oakland)
# 3  06001     Alameda  00112                       Alameda County (Northwest)--Oakland City (Oakland Hills)
# 4  06001     Alameda  00113            Alameda County (West)--Oakland City (Elmhurst/Central/East Oakland)
# 5  06001     Alameda  00114      Alameda County (West)--San Leandro, Alameda, Emeryville & Piedmont Cities
# 6  06001     Alameda  00115                            Alameda County (Northwest)--Castro Valley & Ashland
equivalence_10 <- read.csv(EQUIVALENCE_10, colClasses = c(PUMARC = "character",COUNTY = "character"))
equivalence_20 <- read.csv(EQUIVALENCE_20, colClasses = c(PUMARC = "character",COUNTY = "character"))
print(paste("Read",nrow(equivalence_10),"rows from",EQUIVALENCE_10))
print(paste("Read",nrow(equivalence_20),"rows from",EQUIVALENCE_20))

# acs5 2017-21 uses PUMA (which is PUMA10)
# acs5 2018-22 uses both with PUMA10 and PUMA20
if (argv$survey == "acs5") {
    if (argv$year < 2022) {
        stop("acs5 before 2022 is not supported by this script")
    }
    if (argv$year == 2022) {
        # handle both PUMA10 and PUMA20
        hbayarea <- left_join(hcalif,  equivalence_10, by=c("PUMA10"="PUMARC"), relationship="many-to-one")
        hbayarea <- left_join(hbayarea,equivalence_20, by=c("PUMA20"="PUMARC"), relationship="many-to-one", suffix=c("","_20"))
        # consolidate the _20 version into the _10 (no suffix) version
        hbayarea <- hbayarea %>% 
        mutate(
            County_Name = ifelse(!is.na(County_Name_20),County_Name_20,County_Name),
            COUNTY      = ifelse(!is.na(COUNTY_20     ),COUNTY_20,     COUNTY     ),
            PUMA_Name   = ifelse(!is.na(PUMA_Name_20  ),PUMA_Name_20, PUMA_Name   )) %>%
        select(-County_Name_20, -COUNTY_20, -PUMA_Name_20) %>% 
        filter(!is.na(COUNTY))


        pbayarea <- left_join(pcalif,  equivalence_10, by=c("PUMA10"="PUMARC"), relationship="many-to-one")
        pbayarea <- left_join(pbayarea,equivalence_20, by=c("PUMA20"="PUMARC"), relationship="many-to-one", suffix=c("","_20"))
        # consolidate the _20 version into the _10 (no suffix) version
        pbayarea <- pbayarea %>% 
        mutate(
            County_Name = ifelse(!is.na(County_Name_20),County_Name_20,County_Name),
            COUNTY      = ifelse(!is.na(COUNTY_20     ),COUNTY_20,     COUNTY     ),
            PUMA_Name   = ifelse(!is.na(PUMA_Name_20  ),PUMA_Name_20,  PUMA_Name  )) %>%
        select(-County_Name_20, -COUNTY_20, -PUMA_Name_20) %>%
        filter(!is.na(COUNTY))
    }
    if (argv$year > 2022) {
        stop("acs5 after 2022 is not *yet* supported by this script")
    }
}
# acs1 2021 uses PUMA (which is PUMA10)
# acs1 2022 uses PUMA (which is PUMA20)
if (argv$survey == "acs1") {
    if (argv$year <= 2021) {
        hbayarea <- left_join(hcalif,equivalence_10, by=c("PUMA"="PUMARC"), relationship="many-to-one")
        pbayarea <- left_join(pcalif,equivalence_10, by=c("PUMA"="PUMARC"), relationship="many-to-one")
    }
    if (argv$year >= 2022) {
        hbayarea <- left_join(hcalif,equivalence_20, by=c("PUMA"="PUMARC"), relationship="many-to-one")
        pbayarea <- left_join(pcalif,equivalence_20, by=c("PUMA"="PUMARC"), relationship="many-to-one")
    }
    hbayarea <- filter(hbayarea, !is.na(COUNTY))
    pbayarea <- filter(pbayarea, !is.na(COUNTY))
}
print(paste("Filtered to ",nrow(hbayarea),"rows for hbayarea"))
print(paste("Filtered to ",nrow(pbayarea),"rows for pbayarea"))

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
