# ACS 2019 PUMS Labor Force by County and Race and Gender.R
# Analyze PUMS data for labor force within Bay Area by county and race and gender 
# 2019 1-year PUMS data
# Labor force includes all employed and unemployed people, but not people out of the labor force (e.g., retired)

# Import Library

suppressMessages(library(dplyr))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2019/pbayarea19.Rdata"
OUTPUT = "M:/Data/Requests/Terry Lee/"
load (PERSON_RDATA)

# Set ESR codes of interest - basically anyone employed or unemployed, but in the labor force
# People excluded are those under 16 and/or not in the labor force (codes "NA" and "6" for ESR)

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "3", #Unemployed
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

person <- pbayarea19 %>% 
  select(PUMA,County_Name,PWGTP,HISP,RAC1P,ESR,SEX) %>%
  filter(ESR %in% ESR_codes) %>% 
    mutate(
  Male=if_else(SEX==1, PWGTP,0L),
  Female=if_else(SEX==2, PWGTP,0L),
  White=if_else((HISP==1 & RAC1P==1),PWGTP,0L),
  Black=if_else((HISP==1 & RAC1P==2),PWGTP,0L),
  Asian_PI=if_else((HISP==1 & RAC1P %in% c(6,7)),PWGTP,0L),
  American_Indian_or_Alaska_Native=if_else((HISP==1 & RAC1P %in% c(3,4,5)),PWGTP,0L),
  All_others=if_else((HISP==1 & RAC1P %in% c(8,9)),PWGTP,0L), # Includes "some other race alone" and "two or more races"
  Hispanic=if_else((HISP>1),PWGTP,0L),
  Total=PWGTP
)

sum.total <- person %>%
  group_by(County_Name) %>%
  summarize(freq = n(), Male=sum(Male), Female=sum(Female),White=sum(White), Black=sum(Black),
            Asian_PI=sum(Asian_PI), American_Indian_or_Alaska_Native=sum(American_Indian_or_Alaska_Native),
            All_Others=sum(All_others),Hispanic=sum(Hispanic), Total=sum(Total))

write.csv(sum.total, paste0(OUTPUT, "ACS PUMS 2019 Labor Force by County and Race.csv"), row.names = FALSE, quote = T)


 