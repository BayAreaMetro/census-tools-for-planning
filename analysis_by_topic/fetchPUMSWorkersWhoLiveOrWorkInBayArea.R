USAGE = "
  Fetch all states PUMS data for the given year and retain records ONLY
  for workers who live or work in the Bay Area.

  Save to M:\\Data\\Census\\PUMS\\PUMS YYYY\\allWorkersWhoLiveOrWorkInBayArea.[RData,csv]

  See also travel-model-one\\utilities\\taz-data-baseyears\\fetch_LEHD_LODES_OD_summarize_county.py
"

# Import Libraries

suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(foreign)
  library(argparser)
})
options(width = 200)
source("../labels/pums_labels.R")

# built-in dataset
data("state")

PUMS_DIR = "M:/Data/Census/PUMS"
PUMA10_FILE = "M:/Data/Census/Geography/tl_2010_06_puma10/tl_2010_06_puma10.dbf"
PUMA20_FILE = "M:/Data/Census/Geography/tl_2020_06_puma20/tl_2020_06_puma20.dbf"


argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="year",    help="Survey year", type="numeric")
# parse the command line arguments
argv <- parse_args(argparser)
stopifnot(argv$year %in% c(2015,2022,2023)) # we only support these right now

# read PUMA
if (argv$year >= 2022) {
    PUMA <- read.dbf(PUMA20_FILE)
    PUMA$STATEFP  <- as.character(PUMA$STATEFP20)
    PUMA$PUMACE   <- as.character(PUMA$PUMACE20)
    PUMA$GEOID    <- as.character(PUMA$GEOID20)
    PUMA$NAMELSAD <- as.character(PUMA$NAMELSAD20)
} else {
    PUMA <- read.dbf(PUMA10_FILE)
    PUMA$STATEFP  <- as.character(PUMA$STATEFP10)
    PUMA$PUMACE   <- as.character(PUMA$PUMACE10)
    PUMA$GEOID    <- as.character(PUMA$GEOID10)
    PUMA$NAMELSAD <- as.character(PUMA$NAMELSAD10)
}

# calculate county FIPS
PUMA <- PUMA %>% mutate(FIPS = substr(GEOID,1,5))
# and 6 digit version of PUMA with one more 0 on the front (effectively country)
PUMA <- PUMA %>% mutate(PUMACE_8len = paste0("006", PUMACE))

# inner join to just Bay Area counties
BAY_AREA_PUMA <- inner_join(PUMA, BAY_AREA_COUNTY_FIPS, by="FIPS") %>%
    select(PUMACE_8len, NAMELSAD)

# add counties as PUMAs
BAY_AREA_PUMA <- bind_rows(
    BAY_AREA_PUMA, 
    BAY_AREA_COUNTY_FIPS %>% 
        mutate(FIPS = paste0("0",FIPS,"00")) %>%
        rename(PUMACE_8len = FIPS, NAMELSAD = COUNTY) %>%
        filter(NAMELSAD != "Napa") # this is a single PUMA so it's duplicative
)
print("str(BAY_AREA_PUMA):")
print(str(BAY_AREA_PUMA))
print(BAY_AREA_PUMA)

PUMS_VARS = c("RT","STATE","PUMA","COW","ESR","POWSP","POWPUMA","JWTRNS")
if (argv$year == 2015) {
    # handle variable names that differed in 2015
    PUMS_VARS <- replace(PUMS_VARS, PUMS_VARS=="STATE", "ST")
    PUMS_VARS <- replace(PUMS_VARS, PUMS_VARS=="JWTRNS", "JWTR")
}
if (argv$year == 2022) {
    # handle variable names that differed in 2015
    PUMS_VARS <- replace(PUMS_VARS, PUMS_VARS=="STATE", "ST")
}
print("PUMS_VARS:")
print(PUMS_VARS)

# all workers who live or work in the bay area
all_workers_bay_area <- data.frame()
for (state_abb in state.abb) {

    pums_state <- get_pums(
        variables=PUMS_VARS,
        state = tolower(state_abb),
        survey = "acs1",
        year = argv$year
    )
    # rename for backwards compat
    if (argv$year == 2015) {
        pums_state <- pums_state %>% rename(STATE = ST, JWTRNS=JWTR)
        # these may be short
        pums_state <- pums_state %>% mutate(
            PUMA = case_when(
                str_length(PUMA) == 5 ~ PUMA,
                str_length(PUMA) == 4 ~ paste0("0",PUMA),
                str_length(PUMA) == 3 ~ paste0("00",PUMA),
                str_length(PUMA) == 2 ~ paste0("000",PUMA),
                str_length(PUMA) == 1 ~ paste0("0000",PUMA)
            ),
            POWPUMA = case_when(
                str_length(POWPUMA) == 5 ~ POWPUMA,
                str_length(POWPUMA) == 4 ~ paste0("0",POWPUMA),
                str_length(POWPUMA) == 3 ~ paste0("00",POWPUMA),
                str_length(POWPUMA) == 2 ~ paste0("000",POWPUMA),
                str_length(POWPUMA) == 1 ~ paste0("0000",POWPUMA)
            ),
            STATE = case_when(
                str_length(STATE) == 2 ~ STATE,
                str_length(STATE) == 1 ~ paste0("0",STATE)
            ),
            POWSP = case_when(
                str_length(POWSP) == 3 ~ POWSP,
                str_length(POWSP) == 2 ~ paste0("0",POWSP),
                str_length(POWSP) == 1 ~ paste0("00",POWSP),
            )
        )
    }
    if (argv$year == 2022) {
        pums_state <- pums_state %>% rename(STATE = ST)
    }
    print("")
    print(paste("Downloaded", nrow(pums_state),"records for",state_abb))

    # filter to
    # 1. Civilian employed, at work
    # 2. Civilian employed, with a job but not at work (e.g., teachers during summer)
    # 4. Armed forces, at work
    # 5. Armed forces, with a job but not at work
    pums_state <- pums_state %>%
        filter(ESR %in% c(1,2,4,5))
    print(paste("  Filtered to workers:", nrow(pums_state)))

    print(pums_state)

    # filter to live or work in California
    pums_state <- pums_state %>%
        filter((STATE == "06") | (POWSP == "006"))
    print(paste("  Filtered to live or work in CA:", nrow(pums_state)))
    if (nrow(pums_state)==0) { next }

    # create HOME_8PUMA = 0 + STATE + PUMA and
    # WORK_8PUMA        = POWSP + POWPUMA
    # both are six digits
    pums_state <- pums_state %>% mutate(
        HOME_8PUMA = paste0("0", STATE, PUMA),
        WORK_8PUMA = paste0(POWSP, POWPUMA)
    )

    # join to bay area pumas to home and work
    pums_state <- left_join(
        pums_state, rename(BAY_AREA_PUMA, HOME_8PUMA=PUMACE_8len, HOME_8PUMA_NAME=NAMELSAD), 
        by="HOME_8PUMA",
        relationship="many-to-one"
    ) 
    pums_state <- left_join(
        pums_state, rename(BAY_AREA_PUMA, WORK_8PUMA=PUMACE_8len, WORK_8PUMA_NAME=NAMELSAD), 
        by="WORK_8PUMA",
        relationship="many-to-one"
    )
    # print("After join:")
    # print(pums_state)

    # filter to live or work in Bay Area
    pums_state <- pums_state %>% filter((!is.na(HOME_8PUMA_NAME) | (!is.na(WORK_8PUMA_NAME))))
    print(paste("  Filtered to live or work in Bay Area:", nrow(pums_state)))

    if (nrow(pums_state) > 0) {
        all_workers_bay_area <- bind_rows(all_workers_bay_area, pums_state)
        print(paste("Total rows:",nrow(all_workers_bay_area)))
    }
}

# save it as rdata
OUTPUT_DIR <- file.path(PUMS_DIR, paste("PUMS",argv$year))

save(all_workers_bay_area, file = file.path(OUTPUT_DIR, "allWorkersWhoLiveOrWorkInBayArea.RData"))
print(paste("Wrote",file.path(OUTPUT_DIR, "allWorkersWhoLiveOrWorkInBayArea.RData")))

write.csv(all_workers_bay_area, file = file.path(OUTPUT_DIR, "allWorkersWhoLiveOrWorkInBayArea.csv"), row.names = FALSE)
print(paste("Wrote",file.path(OUTPUT_DIR, "allWorkersWhoLiveOrWorkInBayArea.csv")))


