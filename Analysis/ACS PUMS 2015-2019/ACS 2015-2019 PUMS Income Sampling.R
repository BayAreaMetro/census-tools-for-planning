# ACS 2015-2019 PUMS Income Sampling.R
# Sample income from PUMS weighted distribution for use in getting discrete income values from categorical data

# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)

# Bring in data

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015-19/hbayarea1519.Rdata"
load (HH_RDATA)

bay_income <- hbayarea1519 %>% 
  mutate(adjustment = ADJINC/1000000,
    income=HINCP*adjustment) %>% 
  filter(!is.na(income))

discrete_income <- function(income_imputed){
  temp <- bay_income %>% 
    filter(
      case_when(
        income_imputed==1               ~ .$income<25000,
        income_imputed==2               ~ .$income>=25000 & .$income<50000,
        income_imputed==3               ~ .$income>=50000 & .$income<75000,
        income_imputed==4               ~ .$income>=75000 & .$income<100000,
        income_imputed==5               ~ .$income>=100000 & .$income<150000,
        income_imputed==6               ~ .$income>=150000 & .$income<200000,
        income_imputed==7               ~ .$income>=200000 & .$income<250000,
        income_imputed==8               ~ .$income>=250000))
value <- sample(temp$income,replace = T,size = 1,prob = temp$WGTP)
return(value)
}

income_range <- c(1,2,3,4,5,6,7,8)
record       <- c(10,20,30,40,50,60,70,80)
trial <- data.frame(record,income_range)

trial2 <- trial %>% 
  rowwise() %>% 
  mutate(new_income=discrete_income(income_range))
