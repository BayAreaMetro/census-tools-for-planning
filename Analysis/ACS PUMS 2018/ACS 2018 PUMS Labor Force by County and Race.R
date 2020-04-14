# ACS 2018 PUMS Labor Force by County and Race.R
# Analyze PUMS data for labor force within BART service area by county and race 
# 2018 1-year PUMS data

# Import Library

suppressMessages(library(dplyr))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2018/pbayarea18.Rdata"
OUTPUT = "M:/Data/Requests/BART/"
load (PERSON_RDATA)

# Set up four counties in BART service area for reference

BART_counties <- c("Alameda",
                   "Contra Costa",
                   "San Francisco",
                   "San Mateo")

# Set ESR codes of interest - basically anyone employed or unemployed, but in the labor force
# People excluded are those under 16 and/or not in the labor force (codes "NA" and "6" for ESR)

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "3", #Unemployed
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

person <- pbayarea18 %>% 
  select(PUMA,County_Name,PWGTP,HISP,RAC1P,ESR) %>%
  filter(ESR %in% ESR_codes & County_Name %in% BART_counties) %>% 
    mutate(
  White=if_else((HISP==1 & RAC1P==1),PWGTP,0L),
  Black=if_else((HISP==1 & RAC1P==2),PWGTP,0L),
  All_others=if_else((HISP==1 & RAC1P>=3),PWGTP,0L),       # Includes "some other race alone" and "two or more races"
  Hispanic=if_else((HISP>1),PWGTP,0L),
  Total=PWGTP
)

sum.total <- person %>%
  group_by(County_Name) %>%
  summarize(freq = n(), White=sum(White), Black=sum(Black),Hispanic=sum(Hispanic),All_Others=sum(All_others),Total=sum(Total))

write.csv(sum.total, paste0(OUTPUT, "ACS PUMS 2018 Labor Force by County and Race.csv"), row.names = FALSE, quote = T)


 