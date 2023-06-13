# ACS 2021 PUMS Mean Wage by Quartile for 2023.R
# Analyze PUMS data for mean wages by quartile
# 2021 1-year PUMS data

options(scipen = 999)

# Import Library

suppressMessages(library(dplyr))
library(reldist)
library(stats)

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2021/pbayarea21.Rdata"
OUTPUT = "M:/Data/Requests/Anup Tapase/"
load (PERSON_RDATA)

# CPI values for all of 2021 and for February 2023

cpi2021        <- 309.721
cpi2023        <- 337.173
cpi_correction = cpi2023/cpi2021

# Set ESR codes of interest - basically anyone employed 
# People excluded are those under 16, unemployed, and/or not in the labor force (codes "NA" and "3,6" for ESR)

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

# Filter for people in the above universe and have earnings greater than zero

person <- pbayarea21 %>% 
  select(ESR,PERNP,PWGTP,ADJINC,WKHP,WKWN) %>% 
  mutate(WKHP=as.numeric(WKHP)) %>% 
  filter(ESR %in% ESR_codes,PERNP>0) %>% 
  mutate(earnings2021=PERNP*(ADJINC/1000000), earnings2023=earnings2021*cpi_correction,
         hrs_year=(if_else(WKHP>40,40,WKHP)*WKWN),hrly_wage=earnings2023/hrs_year)

# Calculate quartile thresholds

q1_threshold = wtd.quantile (person$hrly_wage, q=0.25, na.rm = FALSE, weight=person$PWGTP) 
q2_threshold = wtd.quantile (person$hrly_wage, q=0.5, na.rm = FALSE, weight=person$PWGTP) 
q3_threshold = wtd.quantile (person$hrly_wage, q=0.75, na.rm = FALSE, weight=person$PWGTP) 

# Calculate average wages by hour (assuming 2080 hours per year)

final <- person %>% 
  mutate(quartile=case_when(
    hrly_wage<q1_threshold                           ~ paste0("Q1, $0 - $",round(q1_threshold,2)-.01),
    hrly_wage>=q1_threshold & hrly_wage<q2_threshold ~ paste0("Q2, $",round(q1_threshold,2)," - $",round(q2_threshold,2)-.01),
    hrly_wage>=q2_threshold & hrly_wage<q3_threshold ~ paste0("Q3, $",round(q2_threshold,2)," - $",round(q3_threshold,2)-.01),
    hrly_wage>=q3_threshold                          ~ paste0("Q4, $",round(q3_threshold,2),"+"),
    TRUE                                                    ~ "miscoded"
  )) %>% 
  group_by(quartile) %>% 
  summarize(total_persons=sum(PWGTP),med_qrtile_wage=wtd.quantile(hrly_wage,q=0.5,na.rm=false,PWGTP)) 

write.csv(final, paste0(OUTPUT, "ACS PUMS 2021 Mean 2023 Wage by Quartile.csv"), row.names = FALSE, quote = T)


 
