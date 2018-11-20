# Analyze median income for Bay Area worker households, 2016 1-year PUMS data

# Import Libraries

suppressMessages(library(dplyr))
library(reldist)
library(spatstat)

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"
directory = "M:/Data/Requests/Rebecca Long/Quartile Income/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

person <- pbayarea16 %>% 
  select(ESR,SERIALNO) %>% 
  mutate (
  worker=if_else(ESR %in% 1:2 | ESR %in% 4:5,1,0)
)


sum.hh <- person %>%
  group_by(SERIALNO) %>%
  summarize(workers = sum(worker)) 

working_hhs <- left_join(hbayarea16_occ,sum.hh, by="SERIALNO") %>% 
  select(SERIALNO,PUMA,COUNTY,County_Name,ADJINC,HINCP,workers,WGTP, PUMA_Name) %>% 
  filter (workers>0) %>% mutate(
    adjustedinc=HINCP*(ADJINC/1000000)
  )

#income_median <- wtd.quantile (working_hhs$adjustedinc, q=0.50, na.rm = FALSE, weight=working_hhs$WGTP)
#bay <- data.frame("Bay Area",income_median)
#names(bay) <- c("County_Name","median_income")
#row.names(bay) <- NULL

PUMA_median <- working_hhs %>%
  group_by(County_Name, PUMA, PUMA_Name) %>%
  summarize(median_income=weighted.median(adjustedinc,WGTP))

#final <- rbind(county_median,bay)

#write.csv(final, paste0(directory,"ACSPUMS2016_HH_Incomes_Worker_HHs.csv"))

write.csv(PUMA_median, paste0(directory,"ACSPUMS2016_HH_Incomes_Worker_HHs_PUMAs.csv"))
