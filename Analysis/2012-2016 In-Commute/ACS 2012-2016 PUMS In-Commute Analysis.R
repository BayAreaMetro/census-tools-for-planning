# ACS 2012-2016 PUMS In-Commute Analysis.R

# Analyze 2012-2016 Bay Area PUMS data for intra-regional Bay Area workers

suppressMessages(library(dplyr))
SUMMARY_OUT="M:/Data/Requests/Lisa Zorn/ACS 2012-2016 In-Commute/"  # work directory

baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700) # place-of-work PUMAs(POWPUMA) in the Bay Area

# Input person census files

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pbayarea1216.Rdata"

load (PERSON_RDATA)

# Recode na values for mode (allowing for a commuter-only file), and identify workers who live and work in Bay Area

pbayarea1216$JWTR[is.na(pbayarea1216$JWTR)] <- -999999

commuters1216 <- pbayarea1216 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA, JWTR) %>%
  filter (JWTR!=-999999) %>% mutate(
    bayworkers=ifelse(POWPUMA %in% baypowpuma,PWGTP,0)
  )

# Sum and output summaries of total resident workers and workers who both live/work in Bay Area

sum.commuters1216 <- commuters1216 %>%
  group_by(County_Name) %>%
  summarize(freq = n(), bayresworkers=sum(PWGTP), bayliveandwork=sum(bayworkers))

write.csv(sum.commuters1216, paste0(SUMMARY_OUT,"PUMS2012-2016","_intra_regional_commuters.csv"), row.names = FALSE, quote = T)

