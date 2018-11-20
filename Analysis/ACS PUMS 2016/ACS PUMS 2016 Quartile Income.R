# Analyze PUMS data for TIP Person Income Categories, 2016 1-year PUMS data

# Import Libraries

suppressMessages(library(dplyr))
library(reldist)

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2016/hbayarea16_occ.Rdata"

load (HOUSEHOLD_OCC_RDATA)

household <- hbayarea16_occ %>% mutate(           #Adjusted income to constant 2016$
  adjustedinc=HINCP*(ADJINC/1000000)
)

income_median <- wtd.quantile (household$adjustedinc, q=0.75, na.rm = FALSE, weight=household$WGTP) #Upper quartile

print(income_median)



