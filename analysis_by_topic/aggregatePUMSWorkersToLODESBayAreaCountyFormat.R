USAGE = "
  Aggregates PUMS Worker information (from fetchPUMSWorkersWhoLiveOrWorkInBayArea.R)
  to format in M:\\Data\\Census\\LEHD\\Origin-Destination Employment Statistics (LODES)\\LODES_Bay_Area_county_YYYY.csv

  Columns:
     w_state,h_state,w_county,h_county,Total_Workers
     Alabama,California,Outside Bay Area,Alameda,33

  Save to M:\\Data\\Census\\LEHD\\Origin-Destination Employment Statistics (LODES)\\PUMS_Bay_Area_county_YYYY.csv

  See also travel-model-one\\utilities\\taz-data-baseyears\\fetch_LEHD_LODES_OD_summarize_county.py
"

# Import Libraries

suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(argparser)
})
options(width = 200)
source("../labels/pums_labels.R")

PUMS_DIR = "M:/Data/Census/PUMS"

# built-in dataset from tidyverse
#> str(fips_codes)
#'data.frame':   3256 obs. of  5 variables:
# $ state      : chr  "AL" "AL" "AL" "AL" ...
# $ state_code : chr  "01" "01" "01" "01" ...
# $ state_name : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
# $ county_code: chr  "001" "003" "005" "007" ...
# $ county     : chr  "Autauga County" "Baldwin County" "Barbour County" "Bibb County"
data(fips_codes)

argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="year", help="Survey year", type="numeric")
# parse the command line arguments
argv <- parse_args(argparser)

INPUT_FILE  = file.path(PUMS_DIR, paste("PUMS",argv$year), "allWorkersWhoLiveOrWorkInBayArea.RData")
OUTPUT_FILE = file.path("M:/Data/Census/LEHD/Origin-Destination Employment Statistics (LODES)", 
    paste0("PUMS_Bay_Area_county_",argv$year,".csv"))

load(INPUT_FILE)

# HOME_8PUMA = [1 county digit, 0 for USA] + [state 2 digits, 06 = California] + [county 3 digits] + [PUMA digits]

# create fips county 6 digit code for joining
fips_codes <- fips_codes %>% mutate(
    county_fips = paste0("0",state_code,county_code)
)
print(head(fips_codes))

all_workers_bay_area <- all_workers_bay_area %>% mutate(
    HOME_6COUNTY = substr(HOME_8PUMA,1,6),
    WORK_6COUNTY = substr(WORK_8PUMA,1,6)
) 
# join for home
all_workers_bay_area <- left_join(
    all_workers_bay_area,
    select(fips_codes, state_name, county, county_fips) %>% rename(
        HOME_6COUNTY = county_fips, h_state = state_name, h_county = county),
    by="HOME_6COUNTY",
    relationship="many-to-one"
)
# join for work
all_workers_bay_area <- left_join(
    all_workers_bay_area,
    select(fips_codes, state_name, county, county_fips) %>% rename(
        WORK_6COUNTY = county_fips, w_state = state_name, w_county = county),
    by="WORK_6COUNTY",
    relationship="many-to-one"
)

# code a couple simple variables
all_workers_bay_area <- all_workers_bay_area %>% 
    mutate(
        Total_Workers=PWGTP,
        Total_Workers_NonWFH=ifelse(JWTRNS==11,0,PWGTP),  # 11 = WFH
        h_county=ifelse(is.na(HOME_8PUMA_NAME),"Outside Bay Area",h_county),
        w_county=ifelse(is.na(WORK_8PUMA_NAME),"Outside Bay Area",w_county),
        # remove " County" suffix
        h_county=str_replace(h_county, " County",""),
        w_county=str_replace(w_county, " County",""),
    )

print(head(all_workers_bay_area))

# groupby and summarize
workers_grouped <- all_workers_bay_area %>%
    group_by(w_state,h_state,w_county,h_county) %>%
    summarise(
        Total_Workers = sum(Total_Workers),
        Total_Workers_NonWFH = sum(Total_Workers_NonWFH)
    )

write.csv(workers_grouped, file=OUTPUT_FILE, row.names = FALSE, quote=FALSE)
print(paste("Wrote",nrow(workers_grouped),"rows to",OUTPUT_FILE))