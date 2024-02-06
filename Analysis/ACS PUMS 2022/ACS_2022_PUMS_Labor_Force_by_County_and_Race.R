# ACS 2022 PUMS Labor Force by County and Race.R
# Analyze PUMS data for labor force and totals within BART service area by race and gender 
# 2022 1-year PUMS data

# Import Library

suppressMessages(library(dplyr))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2022/pbayarea22.Rdata"
OUTPUT = "M:/Data/Requests/BART/"
load (PERSON_RDATA)

# Set up four counties in BART service area for reference

BART_counties <- c("Alameda",
                   "Contra Costa",
                   "San Francisco",
                   "San Mateo",
                   "Santa Clara")

# Set ESR codes of interest - basically anyone employed or unemployed, but in the labor force
# People excluded are those under 16 and/or not in the labor force (codes "NA" and "6" for ESR)

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "3", #Unemployed
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

# ESR==6 is not in labor force and omitted from this universe

# First summarize total service area population, then available workforce

total_persons <- pbayarea22 %>% 
  select(PUMA,County_Name,PWGTP,HISP,RAC1P,ESR,SEX) %>%
  filter(County_Name %in% BART_counties) %>% 
  mutate(
    Race_RC=case_when(
    HISP==1 & RAC1P==1                            ~ "1_White, Non-Hispanic",
    HISP==1 & RAC1P==2                            ~ "2_Black, Non-Hispanic",
    HISP==1 & RAC1P>=3                            ~ "4_Other, Non-Hispanic", # Includes "some other race alone" and "two or more races"
    HISP>1                                        ~ "3_Hispanic or Latino, any race"),
    Male=if_else(SEX==1,PWGTP,0L),
    Female=if_else(SEX==2,PWGTP,0L)) %>% 
  group_by(Race_RC) %>% 
  summarize(Male=sum(Male),Female=sum(Female)) %>% 
  mutate(Total=Male+Female)


total_workforce <- pbayarea22 %>% 
  select(PUMA,County_Name,PWGTP,HISP,RAC1P,ESR,SEX) %>%
  filter(ESR %in% ESR_codes, County_Name %in% BART_counties) %>% 
  mutate(
    Race_RC=case_when(
      HISP==1 & RAC1P==1                            ~ "1_White, Non-Hispanic",
      HISP==1 & RAC1P==2                            ~ "2_Black, Non-Hispanic",
      HISP==1 & RAC1P>=3                            ~ "4_Other, Non-Hispanic", # Includes "some other race alone" and "two or more races"
      HISP>1                                        ~ "3_Hispanic or Latino, any race"),
    Male=if_else(SEX==1,PWGTP,0L),
    Female=if_else(SEX==2,PWGTP,0L)) %>% 
  group_by(Race_RC) %>% 
  summarize(Male=sum(Male),Female=sum(Female)) %>% 
  mutate(Total=Male+Female)

write.csv(total_persons, paste0(OUTPUT, "ACS PUMS 2022 Total Population by Race and Gender.csv"), row.names = FALSE, quote = T)
write.csv(total_workforce, paste0(OUTPUT, "ACS PUMS 2022 Total Workforce by Race and Gender.csv"), row.names = FALSE, quote = T)


 
