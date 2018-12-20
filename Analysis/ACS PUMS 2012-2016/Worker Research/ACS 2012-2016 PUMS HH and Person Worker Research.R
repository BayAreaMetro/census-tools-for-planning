# ACS 2012-2016 PUMS HH and Person Worker Research.R
# Analyze PUMS data for total workers and households by number of workers, 2016 5-year PUMS data
# Import Libraries

suppressMessages(library(dplyr))

# Set working directory

wd <- "M:/Data/Requests/Lisa Zorn/PUMS Worker Research/"
setwd(wd)

# Input household and person census files, merge them
# Select out needed variables
# Recode as worker or non-worker

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pbayarea1216.Rdata"

load (HOUSEHOLD_RDATA)
load (PERSON_RDATA)

combined <- left_join(pbayarea1216,hbayarea1216, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
                                                      "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPE,ESR,NP,AGEP) %>% mutate(
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
      
person_worker_summary <- combined %>%
  group_by(County_Name) %>%
  summarize(person_worker_total=sum(p_workers)) %>%
  ungroup()

# Find average number of workers in 3+ worker HHs

HH_summary_1 <- combined %>%
  group_by(SERIALNO) %>%
  summarize(worker_total=sum(worker_or_not)) %>%
  ungroup

HH_summary3p <- HH_summary_1 %>%
  filter(worker_total>=3) %>%
  left_join(.,hbayarea1216,by="SERIALNO") %>%
  select(SERIALNO,WGTP,worker_total) %>% 
  summarize(avg3p=weighted.mean(worker_total,WGTP))

mean_workers3P <- as.numeric(HH_summary3p[1,1])              # Save this value in a variable

# Recode number of weighted workers

HH_summary_2 <- left_join(HH_summary_1,hbayarea1216,by="SERIALNO") %>%
  select(SERIALNO,WGTP,worker_total,County_Name) %>% mutate(
    hhworker_weighted = case_when(
      worker_total==0 ~ as.numeric(0*WGTP),                  # Weight adjustment for 0 worker households
      worker_total==1 ~ as.numeric(1*WGTP),                  # Weight adjustment for 1 worker households
      worker_total==2 ~ as.numeric(2*WGTP),                  # Weight adjustment for 2 worker households
      worker_total>=3 ~ as.numeric(mean_workers3P*WGTP)      # Weight adjustment for 3+ worker households
    )
  )

# Summarize workers using household weights, using

HH_worker_summary <- HH_summary_2 %>%
  group_by(County_Name) %>%
  summarize(HH_worker_total=sum(hhworker_weighted))

# Summarize number of households by county and number of HH workers

HH_worker_cat <- HH_summary_2 %>% mutate(
  worker_total_rc=case_when(
    worker_total==0 ~ "0_workers",
    worker_total==1 ~ "1_worker",
    worker_total==2 ~ "2_workers",
    worker_total>=3 ~ "3p_workers"
  )) %>%
  group_by(County_Name,worker_total_rc) %>%
  summarize(total_hhs=sum(WGTP))

# Join person and HH summaries for export

final <- left_join(person_worker_summary,HH_worker_summary, by="County_Name")

# Output csv

write.csv(final, "ACSPUMS2012-2016_Person_Household_Worker_Totals.csv",row.names = FALSE, quote = T)
write.csv(HH_worker_cat, "ACSPUMS2012-2016_Person_Household_Worker_Category.csv",row.names = FALSE, quote = T)




 