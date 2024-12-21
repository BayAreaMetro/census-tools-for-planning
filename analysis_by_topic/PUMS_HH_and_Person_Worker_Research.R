USAGE = "
 Analyze PUMS data for total wrks and households by number of wrks, 5-year or 1-year PUMS data.
 Instituional group quarter records are filtered out.

 Here, wrks are defined by ESR values: 
   1: wrks at work
   2: Job, but not at work
   4: Armed forces at work
   5: Armed forces not at work
 Household wrks are wrks associated with a housing unit where TYPEHUGQ==1

 Reads M:/Data/Census/PUMS/PUMS YYYY[-YY]/[hp]bayarea[YY-YY].Rdata
 Outputs the following files into M:/Data/Census/PUMS/PUMS YYYY[-YY]/summaries/:
 * county_hh_worker_summary_[long,wide].csv

 These are effectively the same, but the wide form has type and hh_wrks moved into columns. Columns:
 * COUNTY           = 5 digit FIPS county
 * County_Name      = Alameda, Contra Costa, etc.
 * type             = one of 'gq' or 'hh'
 * hh_wkrs          = one of 'hh_wrks(14|1245)_(0|1|2|3plus)'
   The first part, (14|1245), refers to which ESR definition is being used to define a worker
   ESR in 1,4 vs ESR in 1,2,4,5.  The second part is a household category indicating the number of those workers.
 * sum_WGTP         = sum of household weights, or number of households (this is 0 for gq)
 * sum_worker_PWGTP = sum of PWGTP for workers, or number of workers
 * avg_workers      = average number of workers per household

Also outputs
* county_hh_size_sumary_wide.csv with columns:
  * COUNTY
  * County_Nam
  * avg_persons.hh.hh_size_4plus
  * sum_WGTP.hh.hh_size_[1,2,3,4plus]
  * sum_PWGTP.gq.hh_size_1
  * sum_PWGTP.hh.hh_size_[1,2,3,4plus]

"

# Import Libraries

suppressMessages({
  library(tidyverse)
  library(argparser)
  library(scales) # for comma()
  library(tidycensus)
})
options(width=500)

argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="survey",  help="Either acs1 or acs5")
argparser <- add_argument(parser=argparser, arg="year",    help="Survey year", type="numeric")
# parse the command line arguments
argv <- parse_args(argparser)
stopifnot(argv$survey %in% c("acs1","acs5"))
stopifnot(argv$year > 2020)

PUMS_ROOT_DIR <- "M:/Data/Census/PUMS"
if (argv$survey == "acs5") {
    PUMS_DIR <- file.path(PUMS_ROOT_DIR, sprintf("PUMS %d-%02d", argv$year-4, argv$year %% 100))
    PUMS_YEAR_STR <- sprintf("%02d%02d", (argv$year-4) %% 100, argv$year %% 100)
} else {
    PUMS_DIR <- file.path(PUMS_ROOT_DIR, sprintf("PUMS %d", argv$year))
    PUMS_YEAR_STR <- sprintf("%02d", argv$year %% 100)
}
print(paste("PUMS_DIR:", PUMS_DIR))
print(paste("PUMS_YEAR_STR:", PUMS_YEAR_STR))


# write to log
dir.create(file.path(PUMS_DIR, "summaries"), showWarnings = FALSE)
run_log <- file.path(PUMS_DIR, "summaries", "PUMS_HH_and_Person_Worker_Research.log")
print(paste("Writing log to",run_log))
sink(run_log, append=FALSE, type = c('output', 'message'))

# Input household and person census files, merge them
# Select out needed variables
# Recode as worker or non-worker

HOUSEHOLD_RDATA = file.path(PUMS_DIR, paste0("hbayarea",PUMS_YEAR_STR,".Rdata"))
PERSON_RDATA    = file.path(PUMS_DIR, paste0("pbayarea",PUMS_YEAR_STR,".Rdata"))

print(paste("Loading",HOUSEHOLD_RDATA))
load (HOUSEHOLD_RDATA)
print(paste("Loading",PERSON_RDATA))
load (PERSON_RDATA)

if ((argv$survey == "acs5") & (argv$year == 2021)) {
  pbayarea <- pbayarea1721
  remove(pbayarea1721)
  hbayarea <- hbayarea1721
  remove(hbayarea1721)
}
if ("PUMA20" %in% colnames(pbayarea)) {
  # rename for backwards compat
  pbayarea <- pbayarea %>% rename(PUMA = PUMA20)
  hbayarea <- hbayarea %>% rename(PUMA = PUMA20)
}
if ("STATE" %in% colnames(pbayarea)) {
  # rename for backwards compat
  pbayarea <- pbayarea %>% rename(ST = STATE)
  hbayarea <- hbayarea %>% rename(ST = STATE)
}

# ESR = Employment status recode
#    b .N/A (less than 16 years old)
#    1 .Civilian employed, at work
#    2 .Civilian employed, with a job but not at work
#    3 .Unemployed
#    4 .Armed forces, at work
#    5 .Armed forces, with a job but not at work
#    6 .Not in labor force
combined <- left_join(
    pbayarea,
    hbayarea, 
    by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY",  "County_Name", "PUMA_Name"),
    relationship="many-to-one"
  ) %>% select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPEHUGQ,ESR,NP,AGEP) %>%
  mutate(
    ESR_in_1245=ifelse(ESR %in% c(1,2,4,5), 1,0),
    ESR_in_14  =ifelse(ESR %in% c(1,  4  ), 1,0),
  ) %>% filter(TYPEHUGQ != 2) %>% # filter out Institutional group quarters
  tibble()

print("combined:")
print(combined, n=10)

# group by households
household_summary <- combined %>%
  group_by(SERIALNO, TYPEHUGQ) %>%
  summarize(
    record_count       = n(),     # should match NP
    hh_sum_ESR_in_1245 = sum(ESR_in_1245),
    hh_sum_ESR_in_14   = sum(ESR_in_14),
    NP                 = first(NP),
    WGTP               = first(WGTP),
  ) %>% mutate(
    # option A for household workers: ESR in 1,2,4,5
    hh_wrks1245 = case_when(
      hh_sum_ESR_in_1245 == 0 ~ "hh_wrks_0",
      hh_sum_ESR_in_1245 == 1 ~ "hh_wrks_1",
      hh_sum_ESR_in_1245 == 2 ~ "hh_wrks_2",
      hh_sum_ESR_in_1245 >= 3 ~ "hh_wrks_3plus"),
    # option B for household workers: ESR in 1,2
    hh_wrks14 = case_when(
      hh_sum_ESR_in_14 == 0 ~ "hh_wrks_0",
      hh_sum_ESR_in_14 == 1 ~ "hh_wrks_1",
      hh_sum_ESR_in_14 == 2 ~ "hh_wrks_2",
      hh_sum_ESR_in_14 >= 3 ~ "hh_wrks_3plus"),
    # simple household by size
    hh_size = case_when(
      NP == 1 ~ "hh_size_1",
      NP == 2 ~ "hh_size_2",
      NP == 3 ~ "hh_size_3",
      NP >= 4 ~ "hh_size_4plus",
    ),
    # convert to more readable string
    type = case_when(
      TYPEHUGQ == 1 ~ "hh",
      TYPEHUGQ == 3 ~ "gq"),
    # this can be summed for households
    WGTP_over_NP       = WGTP/NP, 
  )
print("household_summary:")
print(household_summary, n=10)

# check this
stopifnot(all(household_summary$NP == household_summary$record_count))

# add household vars back to combined
combined <- left_join(
  combined,
  select(household_summary, -record_count, -NP, -WGTP),
  by=c("SERIALNO","TYPEHUGQ"),
  relationship="many-to-one"
)

print("combined with household vars:")
print(combined, n=10)

full_county_summary_long = tibble()
full_county_summary_wide = tibble()
# for each of the definitions of household workers
for (worker_def in c("ESR_in_14","ESR_in_1245")) {
  print(paste("Summarizing for worker definition:",worker_def))

  if (worker_def == "ESR_in_14") {
    hh_wrks       <- "hh_wrks14"
    hh_sum_ESR    <- "hh_sum_ESR_in_14"
  } else if (worker_def == "ESR_in_1245") {
    hh_wrks       <- "hh_wrks1245"
    hh_sum_ESR    <- "hh_sum_ESR_in_1245"
  }

  # rename variables without the ESR definition
  combined_renames <- combined %>%
    # rename ESR_in_14 => ESR_is_worker, hh_wrks14 => hh_wrks, hh_sum_ESR_in_14 => hh_sum_ESR
    rename(
      ESR_is_worker = sym(worker_def),
      hh_wrks       = sym(hh_wrks),
      hh_sum_ESR    = sym(hh_sum_ESR),
    )
  print("combined_renames:")
  print(combined_renames, n=10)

  county_summary <- combined_renames %>%
    group_by(COUNTY, County_Name, type, hh_wrks) %>%
    summarize(
      sum_WGTP         = sum(WGTP_over_NP),               # household weight
      sum_worker_PWGTP = sum(ESR_is_worker*PWGTP),        # PGWT for workers
      avg_workers      = weighted.mean(hh_sum_ESR,WGTP_over_NP),  # weighted mean of number of workers per household
    )

  print("county_summary:")
  print(county_summary)  

  # set to or add to full_county_summary_long
  if (worker_def == "ESR_in_14") {
    full_county_summary_long <- mutate(county_summary, hh_wrks=str_replace(hh_wrks, "hh_wrks", "hh_wrks14"))
  } else if (worker_def == "ESR_in_1245") {
    full_county_summary_long <- 
      rbind(full_county_summary_long, mutate(county_summary, hh_wrks=str_replace(hh_wrks, "hh_wrks", "hh_wrks1245")))
  }
  
  county_summary_wide <- county_summary %>% 
    pivot_wider(
      names_from =c(type, hh_wrks),
      values_from=c(avg_workers, sum_WGTP, sum_worker_PWGTP),
      names_sep  =".",
    )

  print("county_summary_wide:")
  print(county_summary_wide)

  # note that the average workers per household with 0, 1 or 2 workers is 0, 1 or 2 so these columns can be dropped;
  # this is really only useful for >3 workers per household
  stopifnot(all(is.na(county_summary_wide$avg_workers.gq.hh_wrks_0)))
  stopifnot(all(is.na(county_summary_wide$avg_workers.gq.hh_wrks_1)))
  stopifnot(all(county_summary_wide$avg_workers.hh.hh_wrks_0 == 0))
  stopifnot(all(county_summary_wide$avg_workers.hh.hh_wrks_1 == 1))
  stopifnot(all(county_summary_wide$avg_workers.hh.hh_wrks_2 == 2))
  # household weights are 0 for GQ so these columns can also be dropped
  stopifnot(all(county_summary_wide$sum_WGTP.gq.hh_wrks_0 == 0))
  stopifnot(all(county_summary_wide$sum_WGTP.gq.hh_wrks_1 == 0))
  # 0 worker household will have sum_worker_PWGTP==0
  stopifnot(all(county_summary_wide$sum_worker_PWGTP.gq.hh_wrks_0 == 0))
  stopifnot(all(county_summary_wide$sum_worker_PWGTP.hh.hh_wrks_0 == 0))

  county_summary_wide <- county_summary_wide %>%
    select(
      -avg_workers.gq.hh_wrks_0,
      -avg_workers.gq.hh_wrks_1, 
      -avg_workers.hh.hh_wrks_0,
      -avg_workers.hh.hh_wrks_1,
      -avg_workers.hh.hh_wrks_2,
      -sum_WGTP.gq.hh_wrks_0,
      -sum_WGTP.gq.hh_wrks_1,
      -sum_worker_PWGTP.gq.hh_wrks_0, 
      -sum_worker_PWGTP.hh.hh_wrks_0)
  print("county_summary_wide:")
  print(county_summary_wide)

  # set to or add to full_county_summary_long
  if (worker_def == "ESR_in_14") {
    # replacement function
    replace_hh_wrks <- function(name) { gsub("hh_wrks", "hh_wrks14", name) }

    full_county_summary_wide <- rename_with(county_summary_wide, replace_hh_wrks, contains("hh_wrks"))
  } else if (worker_def == "ESR_in_1245") {
    # replacement function
    replace_hh_wrks <- function(name) { gsub("hh_wrks", "hh_wrks1245", name) }

    full_county_summary_wide <- left_join(
      full_county_summary_wide,
      rename_with(county_summary_wide, replace_hh_wrks, contains("hh_wrks"))
    )
  }
}

output_file <- file.path(PUMS_DIR, "summaries", "county_hh_worker_summary_long.csv")
write.csv(full_county_summary_long, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))

output_file <- file.path(PUMS_DIR, "summaries", "county_hh_worker_summary_wide.csv")
write.csv(full_county_summary_wide, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))

# Compare with ACS tabular summar of households by workers

simplify_col <- function(name) { gsub("sum_WGTP.hh.", "", name) }

print("County summary:")
print(full_county_summary_wide %>% 
  select(COUNTY, County_Name, starts_with("sum_WGTP.")) %>% 
  rename_with(simplify_col, starts_with("sum_WGTP.hh.")) %>%
  mutate(across(starts_with("hh_wrks"), ~ comma(.))))

censuskey    <- readLines("M:/Data/Census/API/api-key.txt")
baycounties  <- c("01","13","41","55","75","81","85","95","97")
state_code   <- "06"

ACS_county <- tidycensus::get_acs(
  geography = "county", 
  variables = c(
    hhwrks0_   = "B08202_002", # 0-worker HH
    hhwrks1_   = "B08202_003",	# 1-worker HH
    hhwrks2_   = "B08202_004",	# 2-worker HH
    hhwrks3p_  = "B08202_005"  # 3+-worker HH
  ),
  state = state_code, county=baycounties,
  year=argv$year,
  output="wide",
  survey = argv$survey,
  key = censuskey) %>% select(!ends_with('_M'))

print("ACS Table Summary:")
print(ACS_county %>% 
  arrange(GEOID) %>%
  mutate(across(ends_with("_E"), ~ comma(.))))

########################################################
# household by size summary
county_summary <- combined %>%
  group_by(COUNTY, County_Name, type, hh_size) %>%
  summarize(
    sum_WGTP         = sum(WGTP_over_NP),  # household weight
    sum_PWGTP        = sum(PWGTP),         # PGWT for household members
    avg_persons      = weighted.mean(NP,WGTP_over_NP),  # weighted mean of number of persons per household
  )

  print("county_summary:")
  print(county_summary)

  county_summary_wide <- county_summary %>% 
    pivot_wider(
      names_from =c(type, hh_size),
      values_from=c(avg_persons, sum_WGTP, sum_PWGTP),
      names_sep  =".",
    )
  print("county_summary_wide:")
  print(county_summary_wide)

# select out columns that aren't useful
county_summary_wide <- county_summary_wide %>% select(
  -avg_persons.gq.hh_size_1, # all NaN
  -avg_persons.hh.hh_size_1, # all 1
  -avg_persons.hh.hh_size_2, # all 2
  -avg_persons.hh.hh_size_3, # all 3
  -sum_WGTP.gq.hh_size_1,    # all 0
)
output_file <- file.path(PUMS_DIR, "summaries", "county_hh_size_summary_wide.csv")
write.csv(county_summary_wide, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))
