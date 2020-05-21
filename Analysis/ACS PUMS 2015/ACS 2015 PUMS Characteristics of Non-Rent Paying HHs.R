# ACS 2015 PUMS Characteristics of Non-Rent Paying HHs.R
# Analyze PUMS data for characteristics of households that don't pay rent
# 2015 1-year PUMS data

# Import Libraries

suppressMessages(library(tidyverse))
library(questionr)

# Input HH PUMS file and set working directory

HH_RDATA = "M:/Data/Census/PUMS/PUMS 2015/hbayarea15.Rdata"
P_RDATA   = "M:/Data/Census/PUMS/PUMS 2015/pbayarea15.Rdata"
WD = "C:/Users/sisrael/Documents/GitHub/regional_forecast/housing_income_share_metric"
setwd(WD)

# Set CPI values for 2000 and 2015 to inflate household income values, set up 2015-inflated breakpoints

CPI2000      <- 180.20
CPI2015      <- 258.57
CPI_ratio    <- CPI2015/CPI2000 # 2015 CPI/2000 CPI
break1       <- CPI_ratio*30000                   # First income break point for 2000$
break2       <- CPI_ratio*60000                   # Second income break point for 2000$
break3       <- CPI_ratio*100000                  # Third income break point for 2000$

# Bring in HH and person data

load (HH_RDATA) 
load (P_RDATA)

# Recode data

household <- hbayarea15 %>% 
  filter(TEN %in% c(3,4) & !is.na(TEN)) %>% mutate(
  Tenure=case_when(
    TEN==3              ~ "Paid Rent",
    TEN==4              ~ "Rent Free"
  ),
  Moved=case_when(
    MV==1 ~"1_12 months or less",
    MV==2 ~"2_13 to 23 months",
    MV==3 ~"3_2 to 4 years",
    MV==4 ~"4_5 to 9 years",
    MV==5 ~"5_10 to 19 years",
    MV==6 ~"6_20 to 29 years", 
    MV==7 ~"7_30 years or more"
 ),
 Type=case_when(
   HHT==1  ~ "1_Married couple",
   HHT==2  ~ "2_Family, no wife present",
   HHT==3  ~ "3_Family, no husband present",
   HHT==4  ~ "4_Nonfamily male alone",
   HHT==5  ~ "5_Nonfamily male not alone", 
   HHT==6  ~ "6_Nonfamily female alone",
   HHT==7  ~ "7_Nonfamily female not alone"
 ),
  adjustedinc=HINCP*(ADJINC/1000000),                             # Adjusted income to constant 2015$ 
  Quartile=case_when(
    adjustedinc <  break1                         ~ "IncomeQ1",  # Income below 30k in 2000$
    adjustedinc >= break1 & adjustedinc < break2  ~ "IncomeQ2",  # Income 30-60k in 2000$
    adjustedinc >= break1 & adjustedinc < break3  ~ "IncomeQ3",  # Income 60-100k in 2000$
    adjustedinc >  break3                         ~ "IncomeQ4"   # Income above 100k in 2000$
  ),
  own_cost  =if_else(is.na(SMOCP),0L,SMOCP),                      # Owner costs if SMOCP is not NA
  rent_cost =if_else(is.na(GRNTP),0L,GRNTP),                      # Renter costs if GRNTP is not NA
  total_cost=own_cost + rent_cost                                 # Sum of renter and owner costs
  ) %>% 
  filter(!is.na(TEN)) %>%                                         # Keep cases that are not GQ/vacant 
  select(SERIALNO,PUMA,PUMA_Name,County_Name,HINCP,ADJINC, TEN,adjustedinc,
         WGTP,own_cost,rent_cost,total_cost,Quartile,Tenure,BLD,
         BUS,BATH,HHL,HHT,MV,Moved,Type,TYPE,NP)      # Select out relevant variables

# Join household file with selected person variables

person <- pbayarea15 %>% 
  select(SERIALNO,PUMA, SPORDER, AGEP) %>% 
  filter(SPORDER==1)

combined <- left_join(household,person,by=c("SERIALNO","PUMA")) %>% mutate(
  Householder_Age=case_when(
    AGEP<20             ~ "1_Under 20",
    AGEP>=20 & AGEP<45  ~ "2_20 to 44",
    AGEP>=45 & AGEP<65  ~ "3_45 to 64",
    AGEP>=65            ~ "4_65 and older"  
  )
)

# Produce crosstabulations

sum1 <- data.frame(wtd.table(household$Tenure,household$Quartile,weights=household$WGTP))
sum2 <- data.frame(wtd.table(household$Tenure,household$Type,weights=household$WGTP))
sum3 <- data.frame(wtd.table(household$Tenure,household$BLD,weights=household$WGTP))
sum4 <- data.frame(wtd.table(household$Tenure,household$County_Name,weights=household$WGTP))
sum5 <- data.frame(wtd.table(household$Tenure,household$Moved,weights=household$WGTP))
sum6 <- data.frame(wtd.table(combined$Tenure,combined$Householder_Age,weights=household$WGTP))

final <- rbind(sum1,sum2,sum3,sum4,sum5,sum6)
