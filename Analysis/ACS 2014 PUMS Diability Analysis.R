# Analyze 2014 PUMS data for race by disability and household income by disability. Also a commuter tab.

# Import Libraries

suppressMessages(library(dplyr))

# Input occupied household and person census files and output location for CSVs

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2014/hbayarea14_occ.Rdata"
PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2014/pbayarea14.Rdata"

SUMMARY_OUT = "M:/Data/Requests/Alix Bockelman/"

load (HOUSEHOLD_OCC_RDATA)
load (PERSON_RDATA)

# Delete redundant variables for merging

pbayarea14_1 <- pbayarea14

pbayarea14_1$ADJINC <- pbayarea14_1$PUMA <- pbayarea14_1$COUNTY <- pbayarea14_1$County_Name <- 
  pbayarea14_1$PUMA_Name <- pbayarea14_1$RT <- pbayarea14_1$ST <- NULL

# Merge person and household databases by serial number and delete row names field

merged <- merge (pbayarea14_1,hbayarea14_occ, by="SERIALNO")
row.names(merged) <- NULL

# Adjust income to constant year 2014 dollars

merged$adjustment=merged$ADJINC/1000000
merged$Adjustedincome=merged$HINCP*merged$adjustment

# Select important variables

merged1 <- merged %>%
    select (SERIALNO,PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, Adjustedincome, 
            WGTP, DIS, HISP, RAC1P, HINCP, ADJINC)

pbayarea14_2 <- pbayarea14 %>%
    select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, DIS, HISP, RAC1P)

# Recode race/Hispanic variables, income variables, disability, and use person weight for values

total_persons <- pbayarea14_2 %>% mutate(
  White=ifelse((HISP==1 & RAC1P==1),PWGTP,0),
  Black=ifelse((HISP==1 & RAC1P==2),PWGTP,0),
  Asian=ifelse((HISP==1 & RAC1P==6),PWGTP,0),
  AIAN=ifelse((HISP==1 & (RAC1P>=3 & RAC1P<=5)),PWGTP,0),
  NHPI=ifelse((HISP==1 & RAC1P==7),PWGTP,0),
  Other=ifelse((HISP==1 & RAC1P>=8),PWGTP,0),
  Hispanic=ifelse((HISP>1),PWGTP,0),
  Total=PWGTP,
  Disability=ifelse((DIS==1), "Disabled", "Not Disabled")) 
 
household_persons <- merged1 %>% mutate(
  Less_25=ifelse(Adjustedincome<25000,PWGTP,0),
  GT25_50=ifelse((Adjustedincome>=25000 & Adjustedincome<50000),PWGTP,0),
  GT50_75=ifelse((Adjustedincome>=50000 & Adjustedincome<75000),PWGTP,0),
  GT75_100=ifelse((Adjustedincome>=75000 & Adjustedincome<100000),PWGTP,0),
  GT100_150=ifelse((Adjustedincome>=100000 & Adjustedincome<150000),PWGTP,0),
  GT150=ifelse(Adjustedincome>150000,PWGTP,0),
  Total=PWGTP,
  Disability=ifelse((DIS==1), "Disabled", "Not Disabled")) 
  
# Sum and output summaries, race and income by disability status

sum.race <- total_persons %>%
  group_by(County_Name, Disability) %>%
  summarise(freq = n(), White = sum(White), Black = sum(Black), Asian = sum(Asian), AIAN = sum(AIAN),
            NHPI = sum(NHPI), Other = sum(Other), Hispanic = sum(Hispanic), Total = sum(Total))

write.csv(sum.race, paste0(SUMMARY_OUT, "PUMS2014_Race_Disability.csv"), row.names = FALSE, quote = T)

sum.income <- household_persons %>%
  group_by(County_Name, Disability) %>%
  summarise(freq = n(), Less_25 = sum(Less_25), GT25_50 = sum(GT25_50), GT50_75 = sum(GT50_75), 
            GT75_100 = sum(GT75_100), GT100_150 = sum(GT100_150), GT150 = sum(GT150), Total = sum(Total))

write.csv(sum.income, paste0(SUMMARY_OUT, "PUMS2014_Income_Disability.csv"), row.names = FALSE, quote = T)

# Now sum information on commuters by disability

commuters <- pbayarea14 %>%
  filter(JWTR>=1) %>%
  select(SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, DIS, JWTR, JWRIP) %>% mutate(
  drivealone=ifelse((JWTR==1 & JWRIP==1),PWGTP,0),
  carpool=ifelse((JWTR==1 & JWRIP>1),PWGTP,0),
  transit=ifelse((JWTR>=2 & JWTR<=6),PWGTP,0),
  walk=ifelse(JWTR==10,PWGTP,0),
  bike=ifelse(JWTR==9,PWGTP,0),
  athome=ifelse(JWTR==11,PWGTP,0),
  other=ifelse((JWTR==7 | JWTR==8 | JWTR==12),PWGTP,0),
  Total=PWGTP,
  Disability=ifelse(DIS==1, "Disabled", "Not Disabled")) 
  
sum.commuters <- commuters %>%
  group_by(County_Name, Disability) %>%
  summarise(freq = n(), drivealone = sum(drivealone), carpool = sum(carpool), transit = sum(transit), 
            walk = sum(walk), bike = sum(bike), athome = sum(athome), other = sum(other), Total = sum(Total))
  
write.csv(sum.commuters, paste0(SUMMARY_OUT, "PUMS2014_Commuters_Disability.csv"), row.names = FALSE, quote = T)
