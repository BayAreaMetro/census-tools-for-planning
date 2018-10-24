# ACS 2016 PUMS Commuters by HH Income and Mode.R
# Analyze PUMS data for TIP Person Income Categories and By Mode, 2016 1-year PUMS data
# Import Libraries

suppressMessages(library(dplyr))

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"
OUTPUT = "M:/Data/Requests/Randy Rentschler/2016 Mode Income/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

person <- left_join(pbayarea16,hbayarea16_occ,by=c("SERIALNO","PUMA","COUNTY","County_Name","ADJINC")) %>%
 filter(!is.na(TEN)) %>%                                                    # Remove GQ folks for HHs only, allowing income calculation
  select(SERIALNO,PUMA,COUNTY,County_Name,ADJINC,HINCP,PWGTP,JWTR,JWRIP,TEN) %>% mutate(
    adjustedinc=HINCP*(ADJINC/1000000)
  )

person1 <- person %>% 
  filter(JWTR!="") %>% mutate(
  Zero_25=ifelse(adjustedinc<25000,PWGTP,0),
  GT25_50=ifelse(adjustedinc>=25000 & adjustedinc<50000,PWGTP,0),
  GT50_75=ifelse(adjustedinc>=50000 & adjustedinc<75000,PWGTP,0),
  GT75_100=ifelse(adjustedinc>=75000 & adjustedinc<100000,PWGTP,0),
  GT100_150=ifelse(adjustedinc>=100000 & adjustedinc<150000,PWGTP,0),
  GT150=ifelse(adjustedinc>=150000,PWGTP,0),
  Total=PWGTP,
  JTW=if_else(JWTR==1 & JWRIP==1,"Drive Alone",
              if_else(JWTR==1 & JWRIP==2, "Carpool2",
                      if_else(JWTR==1 & JWRIP>=3, "Carpool3p",
                              if_else(JWTR>=2 & JWTR<=6,"Transit",
                                      if_else(JWTR==7 | JWTR==8,"Other",
                                              if_else(JWTR==9,"Bicycle",
                                                      if_else(JWTR==10,"Walk",
                                                              if_else(JWTR==11,"AtHome",
                                                                      if_else(JWTR==12,"Other","Mistake")))))))))
)

sum.county <- person1 %>%
  group_by(County_Name,JTW) %>%
  summarize(freq = n(), Zero_25=sum(Zero_25), GT25_50=sum(GT25_50),GT50_75=sum(GT50_75), GT75_100=sum(GT75_100),
            GT100_150=sum(GT100_150), GT150=sum(GT150), Total=sum(Total))

write.csv(sum.county, paste0(OUTPUT, "ACSPUMS2016_Persons_HHIncome_Mode.csv"), row.names = FALSE, quote = T)



 