# ACS 2012-2016 PUMS Persons by Poverty Status and Minority and Housing Expense.R
# Analyze PUMS data for poverty status by minority and housing expense, 2016 5-year PUMS data
# Import Libraries

suppressMessages(library(dplyr))

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/pbayarea1216.Rdata"
OUTPUT = "M:/Data/Requests/Vikrant Sood/CASA/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

# Join household and person files, sort by minority,income/poverty ratio, share of income on housing
# Create intersection of minority,less than 200 percent income/poverty, and greater than 30 percent income on housing
# Do the same for greater than 50 percent spent on housing
# If conditions create intersection of all those variables, assign PUMS weight, otherwise assign zero


person <- left_join(pbayarea1216,hbayarea1216_occ,by=c("SERIALNO","PUMA","COUNTY","County_Name")) %>%
 filter(TEN==3) %>% # Include only renters
  select(SERIALNO,PUMA,COUNTY,County_Name,GRPIP,POVPIP,PWGTP,TEN,HISP,RAC1P,HINCP) %>%
  mutate(
    total=PWGTP,
    race=if_else((HISP>1 | RAC1P>1),"minority","white"),
    povshare=case_when(
      (POVPIP>=0 & POVPIP<200) ~ "lt200",
      POVPIP>=200 ~ "gt200"
    ),
    rentshare=case_when(
      (GRPIP>=1 & GRPIP<30) ~ "from1_29",
      (GRPIP>=30 & GRPIP<50) ~ "from30_50",
      (GRPIP>=50) ~ "gt50"
      ),
    intersection_gt30=if_else((race=="minority" & povshare=="lt200" & (rentshare=="from30_50" | rentshare=="gt50")),
                              PWGTP,0L),
    intersection_gt50=if_else((race=="minority" & povshare=="lt200" & (rentshare=="gt50")),
                              PWGTP,0L))

# Summarize greater than 30, greater than 50, and totals

sum.person <- person %>%
  mutate(
    intersection_gt30=if_else(is.na(intersection_gt30),0L,intersection_gt30),    # Change NA values to 0, as these PUMA
    intersection_gt50=if_else(is.na(intersection_gt50),0L,intersection_gt50)     # records should be included for totals
  ) %>%
  group_by(PUMA) %>%
  summarize(freq = n(), intersection_gt30=sum(intersection_gt30), intersection_gt50=sum(intersection_gt50),
            total=sum(total)) %>%
  ungroup() %>%
  mutate(sharegt30=intersection_gt30/total,sharegt50=intersection_gt50/total)

# Output csv

write.csv(sum.person, paste0(OUTPUT, "ACSPUMS2012-2016_Persons_Minority_Rent_Poverty_Expense.csv"), row.names = FALSE, quote = T)


 