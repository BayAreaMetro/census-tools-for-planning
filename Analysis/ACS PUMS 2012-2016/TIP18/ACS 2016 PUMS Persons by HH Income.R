# Analyze PUMS data for TIP Person Income Categories, 2016 1-year PUMS data

# Import Libraries

suppressMessages(library(dplyr))

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"
OUTPUT = "M:/Data/Requests/Mallory Atkinson/TIP 2018/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

person <- left_join(pbayarea16,hbayarea16_occ,by=c("SERIALNO","PUMA","COUNTY","County_Name","ADJINC")) %>%
  filter(TEN!="") %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,ADJINC,HINCP,PWGTP) %>% mutate(
    adjustedinc=HINCP*(ADJINC/1000000)
  )

person1 <- person %>% mutate(
  Zero_25=ifelse(adjustedinc<25000,PWGTP,0),
  GT25_50=ifelse(adjustedinc>=25000 & adjustedinc<50000,PWGTP,0),
  GT50_75=ifelse(adjustedinc>=50000 & adjustedinc<75000,PWGTP,0),
  GT75_100=ifelse(adjustedinc>=75000 & adjustedinc<100000,PWGTP,0),
  GT100_150=ifelse(adjustedinc>=100000 & adjustedinc<150000,PWGTP,0),
  GT150=ifelse(adjustedinc>=150000,PWGTP,0),
  Total=PWGTP
)

sum.county <- person1 %>%
  group_by(County_Name) %>%
  summarize(freq = n(), Zero_25=sum(Zero_25), GT25_50=sum(GT25_50),GT50_75=sum(GT50_75), GT75_100=sum(GT75_100),
            GT100_150=sum(GT100_150), GT150=sum(GT150), Total=sum(Total))

write.csv(sum.county, paste0(OUTPUT, "ACSPUMS2016_Persons_HHIncome.csv"), row.names = FALSE, quote = T)



 