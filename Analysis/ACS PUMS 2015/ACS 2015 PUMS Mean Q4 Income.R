# ACS 2015 PUMS Mean Q4 Income.R
# Analyze PUMS data for mean Q4 income in inflated 2000$ to 2015$
# 2015 1-year PUMS data

# Import Libraries

suppressMessages(library(tidyverse))
# library(reldist)   # Keeping as a placeholder in case weighted median is also needed

# Input HH PUMS file and set working directory

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015/hbayarea15.Rdata"
WD = "C:/Users/sisrael/Documents/GitHub/regional_forecast/housing_income_share_metric"
setwd(WD)

# Set CPI values

CPI2000      <- 180.20
CPI2015      <- 258.57
CPI_ratio    <- CPI2015/CPI2000 # 2015 CPI/2000 CPI
Q4_threshold <- CPI_ratio*100000

# Bring in HH data

load (HH_RDATA) 

# Recode data

Q4 <- hbayarea15 %>% mutate(
  adjustedinc=HINCP*(ADJINC/1000000),                 #Adjusted income to constant 2015$ 
  tenure=case_when(
    TEN==1     ~ "Owner",                             # Owned with a mortgage
    TEN==2     ~ "Owner",                             # Owned free and clear
    TEN==3     ~ "Renter",                            # Rented
    TEN==4     ~ "Renter"                             # Occupied without payment of rent
  )
  ) %>% 
  filter(!is.na(TEN) & adjustedinc>=Q4_threshold) %>% # Keep cases that are not GQ/vacant and are above inflated Q4 threshold    
  select(PUMA,PUMA_Name,HINCP,ADJINC, TEN,adjustedinc,tenure,WGTP)  # Select out relevant variables

# Summarize by tenure, then total, concatenate for a final dataset

Q4_tenure <- Q4 %>% 
  group_by(tenure) %>% 
  summarize(mean_income=weighted.mean(adjustedinc,WGTP))

Q4_total <- Q4 %>% 
  summarize(mean_income=weighted.mean(adjustedinc,WGTP))

temp <- data.frame("tenure"="Total")
temp_join <- cbind(temp,Q4_total)  
final <- rbind(Q4_tenure,temp_join)

# Export

write.csv(final,file="ACS PUMS 2015 Q4 Mean Income by Tenure.csv",row.names = FALSE,quote=TRUE)


