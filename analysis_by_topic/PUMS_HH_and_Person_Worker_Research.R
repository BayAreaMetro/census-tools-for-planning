# ACS 2017-2021 PUMS HH and Person Worker Research.R
# Analyze PUMS data for total workers and households by number of workers, 2021 5-year PUMS data
# Import Libraries

suppressMessages(library(tidyverse))

# Set working directory

USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
petrale              <- file.path(USERPROFILE,"Documents","GitHub","petrale","applications")
output               <- file.path(petrale,"travel_model_lu_inputs","2020","Workers")

# Input household and person census files, merge them
# Select out needed variables
# Recode as worker or non-worker

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/hbayarea1721.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"

load (HOUSEHOLD_RDATA)
load (PERSON_RDATA)

combined <- left_join(pbayarea1721,hbayarea1721, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
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
  left_join(.,hbayarea1721,by="SERIALNO") %>%
  select(County_Name,SERIALNO,WGTP,worker_total) %>% 
  group_by(County_Name) %>% 
  summarize(avg3p=weighted.mean(worker_total,WGTP)) %>% 
  ungroup()

HH_summary3p_Bay <- HH_summary_1 %>%   # For full Bay Area
  filter(worker_total>=3) %>%
  left_join(.,hbayarea1721,by="SERIALNO") %>%
  select(County_Name,SERIALNO,WGTP,worker_total) %>% 
  summarize(avg3p=weighted.mean(worker_total,WGTP)) %>% 
  ungroup()

Bay_3p <- as.numeric(HH_summary3p_Bay[1,1])

# Export avg3p values for county

write.csv(HH_summary3p, file.path(output,"ACSPUMS2017-2021_Avg_3p_Workers_County.csv"),row.names = FALSE)

# Recode number of weighted workers

HH_summary_2 <- left_join(HH_summary_1,hbayarea1721,by="SERIALNO") %>%
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

write.csv(final, file.path(output,"ACSPUMS2017-2021_Person_Household_Worker_Totals.csv"),row.names = FALSE, quote = T)
write.csv(HH_worker_cat, file.path(output,"ACSPUMS2017-2021_Person_Household_Worker_Category.csv"),row.names = FALSE, quote = T)
write.csv(combined_HH_GQ, file.path(output,"ACSPUMS2017-2021_Stratified_Person_Worker_Totals.csv"),row.names = FALSE, quote = T)



# Now summarize HH workers with a job, but not at work ("commuters")

combined_commuters <- left_join(pbayarea1721,hbayarea1721, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
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

HH_summary_commuters_2 <- left_join(HH_summary_commuters_1,hbayarea1721,by="SERIALNO") %>%
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

write.csv(HH_summary_commuters_2, file.path(output,"ACSPUMS2017-2021_Household_Commuter_Category.csv"),row.names = FALSE, quote = T)

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
  
write.csv(person_HH_worker_summary, file.path(output,"ACSPUMS2017-2021_HHs_Workers_PWeight.csv"),row.names = FALSE, quote = T)

