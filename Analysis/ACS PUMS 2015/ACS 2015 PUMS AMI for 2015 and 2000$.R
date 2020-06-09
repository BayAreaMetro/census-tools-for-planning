# ACS 2015 PUMS AMI for 2015 and 2000$.R
# Analyze PUMS data for area median income
# 2015 1-year PUMS data

# Import Libraries

suppressMessages(library(tidyverse))
library(reldist)   # Needed for weighted median

# Input HH PUMS file and set working directory

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015/hbayarea15.Rdata"
WD = "C:/Users/sisrael/Documents/GitHub/regional_forecast/housing_income_share_metric"
setwd(WD)

# Set CPI values

CPI2000      <- 180.20
CPI2015      <- 258.57
CPI_ratio    <- CPI2015/CPI2000 # 2015 CPI/2000 CPI

# Bring in HH data

load (HH_RDATA) 

household <- hbayarea15 %>% 
  filter(!is.na(TEN) & TEN!=4) %>% 
  mutate(
    adjustedinc=HINCP*(ADJINC/1000000), 
    persons=case_when(
      NP>8L   ~8L,
      TRUE   ~NP
    )
  ) %>% 
#  group_by(persons) %>% 
  summarize(median_2015=wtd.quantile(adjustedinc,q=0.5,weight=WGTP)) %>% mutate(
    median_2010=median_2015/CPI_ratio,
    ELI_2015=(0.3*median_2015), 
    VLI_2015=(0.5*median_2015),
    ELI_2000=(0.3*median_2015)/CPI_ratio, 
    VLI_2000=(0.5*median_2015)/CPI_ratio
    ) %>% 
  mutate_if(is.numeric,round,0)

print(household)

# Recode data

Q4 <- hbayarea15 %>% 
  filter(TEN!=4) %>% mutate(                          # Remove occupied without payment of rent
  adjustedinc=HINCP*(ADJINC/1000000),                 #Adjusted income to constant 2015$ 
  tenure=case_when(
    TEN==1     ~ "Owner",                             # Owned with a mortgage
    TEN==2     ~ "Owner",                             # Owned free and clear
    TEN==3     ~ "Renter"                            # Rented
  )
  ) %>% 
  filter(!is.na(TEN) & adjustedinc>=Q4_threshold) %>% # Keep cases that are not GQ/vacant and are above inflated Q4 threshold    
  select(PUMA,PUMA_Name,HINCP,ADJINC, TEN,adjustedinc,tenure,WGTP)  # Select out relevant variables

# Summarize by tenure, then total, concatenate for a final dataset

Q4_tenure <- Q4 %>% 
  group_by(tenure) %>% 
  summarize(mean_income_2015dollars=weighted.mean(adjustedinc,WGTP))

Q4_total <- Q4 %>% 
  summarize(mean_income_2015dollars=weighted.mean(adjustedinc,WGTP))

temp <- data.frame("tenure"="Total")
temp_join <- cbind(temp,Q4_total)  
final <- rbind(Q4_tenure,temp_join) %>% mutate(
  mean_income_2000dollars=mean_income_2015dollars/CPI_ratio
)

# Export

write.csv(final,file="ACS PUMS 2015 Q4 Mean Income by Tenure.csv",row.names = FALSE,quote=TRUE)