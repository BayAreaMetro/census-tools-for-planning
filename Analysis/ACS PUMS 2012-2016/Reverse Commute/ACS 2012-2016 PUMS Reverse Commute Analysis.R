# Analyze 2012-2016 PUMS data for reverse commutes. 

# Import Libraries

suppressMessages(library(dplyr))
SUMMARY_OUT="M:/Data/Requests/John Goodwin/Reverse Commute/"

#----------------2012---------------

Year=2012

# Input occupied household and person census files and output location for CSVs

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2012/pbayarea12.Rdata"

load (PERSON_RDATA)

# Select important variables and recode corridors

pbayarea12$POWPUMA[is.na(pbayarea12$POWPUMA)] <- -999999

commuters12 <- pbayarea12 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA) %>% mutate(
    SF_to_SM_SC=ifelse(County_Name=="San Francisco" & (POWPUMA==8100 | POWPUMA==8500),PWGTP,0),
    Oakland_to_CC=ifelse((PUMA %in% 101:105) & POWPUMA==1300,PWGTP,0),
    Marin_to_Sonoma=ifelse(County_Name=="Marin" & POWPUMA==9700,PWGTP,0),
    Bay_to_Yolo_SAC=ifelse((POWPUMA==6700 | POWPUMA==11300),PWGTP,0))

# Sum and output summaries

sum.commuters12 <- commuters12 %>%
  summarize(freq = n(), SF_to_SM_SC = sum(SF_to_SM_SC), Oakland_to_CC=sum(Oakland_to_CC), 
            Marin_to_Sonoma=sum(Marin_to_Sonoma), Bay_to_Yolo_SAC=sum(Bay_to_Yolo_SAC))

write.csv(sum.commuters12, paste0(SUMMARY_OUT,"PUMS",Year,"_Inter_regional_commuters.csv"), row.names = FALSE, quote = T)

#----------------2013---------------

Year=2013

# Input occupied household and person census files and output location for CSVs

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2013/pbayarea13.Rdata"

load (PERSON_RDATA)

# Select important variables and recode corridors

pbayarea13$POWPUMA[is.na(pbayarea13$POWPUMA)] <- -999999

commuters13 <- pbayarea13 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA) %>% mutate(
    SF_to_SM_SC=ifelse(County_Name=="San Francisco" & (POWPUMA==8100 | POWPUMA==8500),PWGTP,0),
    Oakland_to_CC=ifelse((PUMA %in% 101:105) & POWPUMA==1300,PWGTP,0),
    Marin_to_Sonoma=ifelse(County_Name=="Marin" & POWPUMA==9700,PWGTP,0),
    Bay_to_Yolo_SAC=ifelse((POWPUMA==6700 | POWPUMA==11300),PWGTP,0))

# Sum and output summaries

sum.commuters13 <- commuters13 %>%
  summarize(freq = n(), SF_to_SM_SC = sum(SF_to_SM_SC), Oakland_to_CC=sum(Oakland_to_CC), 
            Marin_to_Sonoma=sum(Marin_to_Sonoma), Bay_to_Yolo_SAC=sum(Bay_to_Yolo_SAC))

write.csv(sum.commuters13, paste0(SUMMARY_OUT,"PUMS",Year,"_Inter_regional_commuters.csv"), row.names = FALSE, quote = T)

#----------------2014---------------

Year=2014

# Input occupied household and person census files and output location for CSVs

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2014/pbayarea14.Rdata"

load (PERSON_RDATA)

# Select important variables and recode corridors

pbayarea14$POWPUMA[is.na(pbayarea14$POWPUMA)] <- -999999

commuters14 <- pbayarea14 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA) %>% mutate(
    SF_to_SM_SC=ifelse(County_Name=="San Francisco" & (POWPUMA==8100 | POWPUMA==8500),PWGTP,0),
    Oakland_to_CC=ifelse((PUMA %in% 101:105) & POWPUMA==1300,PWGTP,0),
    Marin_to_Sonoma=ifelse(County_Name=="Marin" & POWPUMA==9700,PWGTP,0),
    Bay_to_Yolo_SAC=ifelse((POWPUMA==6700 | POWPUMA==11300),PWGTP,0))

# Sum and output summaries

sum.commuters14 <- commuters14 %>%
  summarize(freq = n(), SF_to_SM_SC = sum(SF_to_SM_SC), Oakland_to_CC=sum(Oakland_to_CC), 
            Marin_to_Sonoma=sum(Marin_to_Sonoma), Bay_to_Yolo_SAC=sum(Bay_to_Yolo_SAC))

write.csv(sum.commuters14, paste0(SUMMARY_OUT,"PUMS",Year,"_Inter_regional_commuters.csv"), row.names = FALSE, quote = T)

#----------------2015---------------

Year=2015

# Input occupied household and person census files and output location for CSVs

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2015/pbayarea15.Rdata"

load (PERSON_RDATA)

# Select important variables and recode corridors

pbayarea15$POWPUMA[is.na(pbayarea15$POWPUMA)] <- -999999

commuters15 <- pbayarea15 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA) %>% mutate(
    SF_to_SM_SC=ifelse(County_Name=="San Francisco" & (POWPUMA==8100 | POWPUMA==8500),PWGTP,0),
    Oakland_to_CC=ifelse((PUMA %in% 101:105) & POWPUMA==1300,PWGTP,0),
    Marin_to_Sonoma=ifelse(County_Name=="Marin" & POWPUMA==9700,PWGTP,0),
    Bay_to_Yolo_SAC=ifelse((POWPUMA==6700 | POWPUMA==11300),PWGTP,0))

# Sum and output summaries

sum.commuters15 <- commuters15 %>%
  summarize(freq = n(), SF_to_SM_SC = sum(SF_to_SM_SC), Oakland_to_CC=sum(Oakland_to_CC), 
            Marin_to_Sonoma=sum(Marin_to_Sonoma), Bay_to_Yolo_SAC=sum(Bay_to_Yolo_SAC))

write.csv(sum.commuters15, paste0(SUMMARY_OUT,"PUMS",Year,"_Inter_regional_commuters.csv"), row.names = FALSE, quote = T)

#----------------2013---------------

Year=2016

# Input occupied household and person census files and output location for CSVs

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2016/pbayarea16.Rdata"

load (PERSON_RDATA)

# Select important variables and recode corridors

pbayarea16$POWPUMA[is.na(pbayarea16$POWPUMA)] <- -999999

commuters16 <- pbayarea16 %>%
  select (SERIALNO, PUMA, PUMA_Name, COUNTY, County_Name, PWGTP, POWPUMA) %>% mutate(
    SF_to_SM_SC=ifelse(County_Name=="San Francisco" & (POWPUMA==8100 | POWPUMA==8500),PWGTP,0),
    Oakland_to_CC=ifelse((PUMA %in% 101:105) & POWPUMA==1300,PWGTP,0),
    Marin_to_Sonoma=ifelse(County_Name=="Marin" & POWPUMA==9700,PWGTP,0),
    Bay_to_Yolo_SAC=ifelse((POWPUMA==6700 | POWPUMA==11300),PWGTP,0))

# Sum and output summaries

sum.commuters16 <- commuters16 %>%
  summarize(freq = n(), SF_to_SM_SC = sum(SF_to_SM_SC), Oakland_to_CC=sum(Oakland_to_CC), 
            Marin_to_Sonoma=sum(Marin_to_Sonoma), Bay_to_Yolo_SAC=sum(Bay_to_Yolo_SAC))

write.csv(sum.commuters16, paste0(SUMMARY_OUT,"PUMS",Year,"_Inter_regional_commuters.csv"), row.names = FALSE, quote = T)
