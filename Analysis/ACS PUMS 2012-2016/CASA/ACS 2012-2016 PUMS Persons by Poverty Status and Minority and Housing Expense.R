# ACS 2012-2016 PUMS Persons by Poverty Status and Minority and Housing Expense.R
# Analyze PUMS data for Person Income Categories and By Mode, Minority and Age Status, 2016 1-year PUMS data
# Import Libraries

suppressMessages(library(dplyr))

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"
OUTPUT = "M:/Data/Requests/Mallory Atkinson/TIP 2018/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

person <- left_join(pbayarea16,hbayarea16_occ,by=c("SERIALNO","PUMA","COUNTY","County_Name","ADJINC")) %>%
 filter(!is.na(TEN)) %>%                                                    # Remove GQ folks for HHs only, allowing income calculation
  select(SERIALNO,PUMA,COUNTY,County_Name,ADJINC,HINCP,PWGTP,JWTR,JWRIP,TEN,HISP,RAC1P,AGEP) %>% mutate(
    adjustedinc=HINCP*(ADJINC/1000000)
  )

person1 <- person %>% 
  filter(JWTR!="") %>% mutate(
  LT50=if_else(adjustedinc<50000,PWGTP,0L),
  Total=PWGTP,
  Minority=if_else((HISP>1 | RAC1P>1),PWGTP,0L),
  Senior=if_else(AGEP>=65,PWGTP,0L),
  JTW=if_else(JWTR==1 & JWRIP==1,"Roadway(Motorized)",
              if_else(JWTR==1 & JWRIP==2, "Roadway(Motorized)",
                      if_else(JWTR==1 & JWRIP>=3, "Roadway(Motorized)",
                              if_else(JWTR>=2 & JWTR<=6,"Transit",
                                      if_else(JWTR==7 | JWTR==8,"Roadway(Motorized)",
                                              if_else(JWTR==9,"Roadway(Non-Motorized)",
                                                      if_else(JWTR==10,"Roadway(Non-Motorized)",
                                                              if_else(JWTR==11,"AtHome",
                                                                      if_else(JWTR==12,"Other","Mistake")))))))))
)

sum.total <- person1 %>%
  group_by(JTW) %>%
  summarize(freq = n(), LT50=sum(LT50), Minority=sum(Minority),Senior=sum(Senior),Total=sum(Total))

write.csv(sum.total, paste0(OUTPUT, "ACSPUMS2016_Persons_HHIncome_Mode_Minority_Senior.csv"), row.names = FALSE, quote = T)



 