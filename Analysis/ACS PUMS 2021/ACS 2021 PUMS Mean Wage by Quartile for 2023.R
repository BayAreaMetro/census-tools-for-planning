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
# Filter workers working more than 30 hours per week and more than 40 weeks per year

person <- pbayarea21 %>% 
  select(ESR,PERNP,PWGTP,ADJINC,WKHP,WKWN) %>% 
  filter(ESR %in% ESR_codes, PERNP>0,WKHP>30,WKWN>40) %>% 
  mutate(wage2021=PERNP*(ADJINC/1000000), wage2023=wage2021*cpi_correction)

# Calculate quartile thresholds

q1_threshold = wtd.quantile (person$wage2023, q=0.25, na.rm = FALSE, weight=person$PWGTP) 
q2_threshold = wtd.quantile (person$wage2023, q=0.5, na.rm = FALSE, weight=person$PWGTP) 
q3_threshold = wtd.quantile (person$wage2023, q=0.75, na.rm = FALSE, weight=person$PWGTP) 

# Calculate average wages by hour (assuming 2080 hours per year)

final <- person %>% 
  mutate(quartile=case_when(
    wage2023<q1_threshold                                   ~ "Q1",
    wage2023>=q1_threshold & wage2023<q2_threshold          ~ "Q2",
    wage2023>=q2_threshold & wage2023<q3_threshold          ~ "Q3",
    wage2023>=q3_threshold                                  ~ "Q4",
    TRUE                                                    ~ "miscoded"
  )) %>% 
  group_by(quartile) %>% 
  summarize(total_persons=sum(PWGTP),lower_bound=min(wage2023),upper_bound=max(wage2023),
            mean_annual_earnings=weighted.mean(wage2023,PWGTP)) %>% 
  mutate(mean_hourly_wage=mean_annual_earnings/2080)

write.csv(final, paste0(OUTPUT, "ACS PUMS 2021 Mean Wage by Quartile.csv"), row.names = FALSE, quote = T)


 
