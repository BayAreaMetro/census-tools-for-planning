USAGE = "
 Analyze PUMS data for total workers and households by number of workers, 5-year or 1-year PUMS data.

 Here, workers are defined by ESR values: 
   1: Workers at work
   2: Job, but not at work
   4: Armed forces at work
   5: Armed forces not at work
 Household workers are workers associated with a housing unit where TYPEHUGQ==1

 Reads M:/Data/Census/PUMS/PUMS YYYY-YY/[hp]bayarea[YY-YY].Rdata
 Outputs the following files into the same directory:

 * Avg_3p_Workers_County.csv  Contains columns: 
   - County_Name
   - avg3p = average number of workers in households with 3+ workers

 * Person_Household_Worker_Totals.csv  Contains columns: 
   - County_Name
   - person_worker_total = workers using person weights, 
   - HH_worker_total = workers using household weights

 * Person_Household_Worker_Category.csv  Contains columns:
   - County_Name
   - worker_total_rc = one of '0_workers','1_worker','2_workers','3p_workers'
   - total_hhs = sum of household weights

 * Stratified_Person_Worker_Totals.csv  Contains columns:
   - County_Name
   - household_worker_total = person weights for workers in households
   - gq_worker_total = person weights for workers in non-institutionalized GQ (TYPEHUGQ==3)
   - worker_total = household_worker_total + gq_worker_total

 * Household_Commuter_Category.csv  
   This is filtered to workers with ESR == 1 and 4 ONLY. Contains columns:
   - County_Name
   - worker_total_rc = one of '0_workers','1_worker','2_workers','3p_workers'
   - total_hhs = person weights for workers in households
   - total_gq = person weights for workers in non-institutionalized GQ (TYPEHUGQ==3)
   - total_total = household_worker_total + gq_worker_total

 * HHs_Workers_PWeight.csv  Contains columns:
   - County_Name
   - worker_total_rc = one of '0_workers','1_worker','2_workers','3p_workers'
   - total = person weight for workers in these categories

"

# Import Libraries

suppressMessages({
  library(tidyverse)
  library(argparser)
})

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

combined <- left_join(pbayarea,hbayarea, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
                                                      "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPEHUGQ,ESR,NP,AGEP) %>% mutate(
    p_workers=case_when(
      is.na(ESR) ~ 0L,                                     # Create a column of weighted workers, NA is under 16
      ESR==1     ~ PWGTP,                                  # Workers at work
      ESR==2     ~ PWGTP,                                  # Job, but not at work
      ESR==3     ~ 0L,                                     # Unemployed
      ESR==4     ~ PWGTP,                                  # Armed forces at work
      ESR==5     ~ PWGTP,                                  # Armed forces not at work
      ESR==6     ~ 0L                                      # Not in labor force
    ),
    worker_or_not = if_else(p_workers>0L,1L,0L)            # Create a column of workers (1) or not (0)
    )

# Summarize total workers

person_worker_summary <- combined %>%
  group_by(County_Name) %>%
  summarize(person_worker_total=sum(p_workers)) %>%
  ungroup()

# Now stratify by HH and GQ workers and summarize

combined_HH_GQ <- combined %>% mutate(
  household=if_else(TYPEHUGQ==1,p_workers,0L),
  gq=if_else(TYPEHUGQ==3,p_workers,0L),
  total=p_workers
) %>%
  group_by(County_Name) %>%
  summarize(household_worker_total=sum(household),gq_worker_total=sum(gq),worker_total=sum(total)) %>%
  ungroup()
      
# Find average number of workers in 3+ worker HHs by county

HH_summary_1 <- combined %>%
  group_by(SERIALNO) %>%
  summarize(worker_total=sum(worker_or_not)) %>%
  ungroup()

HH_summary3p <- HH_summary_1 %>%   # County-level values
  filter(worker_total>=3) %>%
  left_join(.,hbayarea,by="SERIALNO") %>%
  select(County_Name,SERIALNO,WGTP,worker_total) %>% 
  group_by(County_Name) %>% 
  summarize(avg3p=weighted.mean(worker_total,WGTP)) %>% 
  ungroup()

HH_summary3p_Bay <- HH_summary_1 %>%   # For full Bay Area
  filter(worker_total>=3) %>%
  left_join(.,hbayarea,by="SERIALNO") %>%
  select(County_Name,SERIALNO,WGTP,worker_total) %>% 
  summarize(avg3p=weighted.mean(worker_total,WGTP)) %>% 
  ungroup()

Bay_3p <- as.numeric(HH_summary3p_Bay[1,1])

# Export avg3p values for county

output_file <- file.path(PUMS_DIR,"Avg_3p_Workers_County.csv")
write.csv(HH_summary3p,output_file,row.names = FALSE)
print(paste("Wrote",output_file))

# Recode number of weighted workers

HH_summary_2 <- left_join(HH_summary_1,hbayarea,by="SERIALNO") %>%
  left_join(.,HH_summary3p,by="County_Name") %>% 
  select(SERIALNO,WGTP,worker_total,County_Name,avg3p) %>% mutate(
    hhworker_weighted = case_when(
      worker_total==0 ~ as.numeric(0*WGTP),                  # Weight adjustment for 0 worker households
      worker_total==1 ~ as.numeric(1*WGTP),                  # Weight adjustment for 1 worker households
      worker_total==2 ~ as.numeric(2*WGTP),                  # Weight adjustment for 2 worker households
      worker_total>=3 ~ as.numeric(avg3p*WGTP)      # Weight adjustment for 3+ worker households
    )
  )

# Summarize workers using household weights, 

HH_worker_summary <- HH_summary_2 %>%
  group_by(County_Name) %>%
  summarize(HH_worker_total=sum(hhworker_weighted)) %>% 
  ungroup()

# Summarize number of households by county and number of HH workers

HH_worker_cat <- HH_summary_2 %>% mutate(
  worker_total_rc=case_when(
    worker_total==0 ~ "0_workers",
    worker_total==1 ~ "1_worker",
    worker_total==2 ~ "2_workers",
    worker_total>=3 ~ "3p_workers"
  )) %>%
  group_by(County_Name,worker_total_rc) %>%
  summarize(total_hhs=sum(WGTP)) %>% 
  ungroup()

# Join person and HH summaries for export

final <- left_join(person_worker_summary,HH_worker_summary, by="County_Name")

# Output csv

output_file <- file.path(PUMS_DIR,"Person_Household_Worker_Totals.csv")
write.csv(final, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))

output_file <- file.path(PUMS_DIR,"Person_Household_Worker_Category.csv")
write.csv(HH_worker_cat, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))

output_file <- file.path(PUMS_DIR,"Stratified_Person_Worker_Totals.csv")
write.csv(combined_HH_GQ, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))



# Now summarize HH workers with a job, but not at work ("commuters")

combined_commuters <- left_join(pbayarea,hbayarea, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
                                                      "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPEHUGQ,ESR,NP,AGEP) %>% mutate(
    p_workers=case_when(
      is.na(ESR) ~ 0L,                                     # Create a column of weighted workers, NA is under 16
      ESR==1     ~ PWGTP,                                  # Workers at work
      ESR==2     ~ 0L,                                     # Job, but not at work
      ESR==3     ~ 0L,                                     # Unemployed
      ESR==4     ~ PWGTP,                                  # Armed forces at work
      ESR==5     ~ 0L,                                     # Armed forces not at work
      ESR==6     ~ 0L                                      # Not in labor force
    ),
    worker_or_not = if_else(p_workers>0L,1L,0L)            # Create a column of workers (1) or not (0)
  )

HH_summary_commuters_1 <- combined_commuters %>%
  group_by(SERIALNO) %>%
  summarize(worker_total=sum(worker_or_not)) %>%
  ungroup()

HH_summary_commuters_2 <- left_join(HH_summary_commuters_1,hbayarea,by="SERIALNO") %>%
  select(SERIALNO,WGTP,worker_total,County_Name,TYPEHUGQ) %>% mutate(
  worker_total_rc=case_when(
    worker_total==0 ~ "0_workers",
    worker_total==1 ~ "1_worker",
    worker_total==2 ~ "2_workers",
    worker_total>=3 ~ "3p_workers"
    ),
  household=if_else(TYPEHUGQ==1,WGTP,0L),
  gq=if_else(TYPEHUGQ==3,WGTP,0L))%>%
  group_by(County_Name,worker_total_rc) %>%
  summarize(total_hhs=sum(household),total_gq=sum(gq),total_total=sum(WGTP)) %>% 
  ungroup()

# Output csv

output_file <- file.path(PUMS_DIR,"Household_Commuter_Category.csv")
write.csv(HH_summary_commuters_2, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))

# Create a table of households by number of workers, using the PUMS person weight

person_HH_worker_summary <- combined %>%
  filter(worker_or_not==1) %>%
  left_join(.,HH_summary_1,by="SERIALNO") %>%
  select(SERIALNO,PWGTP,worker_total,County_Name,PUMA) %>% mutate(
  worker_total_rc=case_when(
    worker_total==0 ~ "0_workers",
    worker_total==1 ~ "1_worker",
    worker_total==2 ~ "2_workers",
    worker_total>=3 ~ "3p_workers"
  )) %>%
  group_by(County_Name,worker_total_rc) %>%
  summarize(total=sum(PWGTP)) %>% 
  ungroup()
  
output_file <- file.path(PUMS_DIR, "HHs_Workers_PWeight.csv")
write.csv(person_HH_worker_summary, output_file, row.names = FALSE, quote = T)
print(paste("Wrote",output_file))
