# ACS 2013-2017 PUMS HH and Person Worker Research_For MAZ Worker Correction.R
# Analyze PUMS data for total workers and households by number of workers, using person weights
# Import Libraries

suppressMessages(library(dplyr))

# Set file output location

USERPROFILE      <- Sys.getenv("USERPROFILE")
BOX_US           <- file.path(USERPROFILE,"Box","Modeling and Surveys","Urban Modeling")
Output_Location  <- file.path(BOX_US,"Bay Area Urbansim","Travel Model 2")

# Input household and person census files, merge them
# Select out needed variables
# Recode as worker or non-worker

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/hbayarea1317.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pbayarea1317.Rdata"

load (HOUSEHOLD_RDATA)
load (PERSON_RDATA)

combined <- left_join(pbayarea1317,hbayarea1317, by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", 
                                                      "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,ESR,NP) %>% mutate(
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

# Calculate total number of households by PUMA, excluding vacant housing

PUMA_households <- hbayarea1317 %>% 
  filter(NP>0) %>% 
  group_by(PUMA) %>% 
  summarize(Total_HHs=sum(WGTP))
            
# Summarize total workers using person weight 

HH_workers_pweight <- combined %>%
  group_by(SERIALNO,PUMA,PUMA_Name,COUNTY,County_Name) %>%
  summarize(person_worker_total=sum(p_workers)) %>%
  ungroup()

# Find average number of workers in 3+ worker HHs, keeping ID variables for later

HH_summary_1 <- combined %>%
  group_by(COUNTY,County_Name,PUMA, PUMA_Name, SERIALNO,WGTP) %>%
  summarize(worker_total=sum(worker_or_not)) %>%
  ungroup()

HH_summary3p <- HH_summary_1 %>%
  filter(worker_total>=3) %>%
  left_join(.,hbayarea1317,by=c("COUNTY","County_Name","PUMA","PUMA_Name","SERIALNO","WGTP")) %>%
  select(SERIALNO,WGTP,worker_total,PUMA,PUMA_Name,COUNTY,County_Name) %>%
  group_by(PUMA,PUMA_Name,COUNTY,County_Name) %>% 
  summarize(avg3worker_hhs=weighted.mean(worker_total,WGTP)) %>% 
  ungroup()

# Recode value names for next-step converting list to matrix and using value names as variable labels

HH_wrks <- HH_summary_1 %>% 
  mutate(worker_total=if_else(worker_total>=3,"3P",as.character(worker_total)),
         worker_total=paste0("total_pweight_HHworkers_",worker_total)) %>% 
  select(-WGTP)

# Join value names to summary of person-weighted workers by household. Sum to PUMA and spread to matrix format

Interim1 <- left_join(HH_workers_pweight,HH_wrks,by=c("PUMA","PUMA_Name","COUNTY","County_Name","SERIALNO"))%>% 
  group_by(PUMA,PUMA_Name,COUNTY,County_Name,worker_total) %>% 
  summarize(person_worker_total=sum(person_worker_total)) %>% 
  spread(.,worker_total,person_worker_total) %>% 
  ungroup()

# Join file to include average number of workers in 3-plus worker households (by PUMA)
# Join total HHs by PUMA
# Create variables for number of household workers (as placeholders for next-step calculation)

Interim2 <- left_join(Interim1,HH_summary3p,by=c("PUMA","PUMA_Name","COUNTY","County_Name")) %>% 
  left_join(.,PUMA_households,by="PUMA") %>% 
  mutate(hh_wrks_0=0,
         hh_wrks_1=0,
         hh_wrks_2=0,
         hh_wrks_3_plus=0)

# Calculate households by number of household workers, rounding as necessary
# Start with 1-worker HHs (implied division by 1), 2-worker HHs (divided by 2), 
# 3-plus-worker HHs are divided by PUMA average for 3+-worker households
# Subtract the 1-,2-,and 3+-worker households from total HHs to yield 0-worker HHs.

Final <- Interim2 %>% 
  mutate(hh_wrks_1=total_pweight_HHworkers_1,
         hh_wrks_2=round(total_pweight_HHworkers_2/2),
         hh_wrks_3_plus=round(total_pweight_HHworkers_3P/avg3worker_hhs),
         hh_wrks_0=Total_HHs-(hh_wrks_1+hh_wrks_2+hh_wrks_3_plus))

# Output file
  
write.csv(Final, file.path(Output_Location,"ACSPUMS2013-2017_HHs_Workers_PWeight.csv"),row.names = FALSE, quote = T)

