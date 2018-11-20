# Analyze PUMS data for TIP Person Income Categories, 2012-2016 5-year PUMS data

# Import Libraries

suppressMessages(library(dplyr))
library(reldist)

# Input occupied household and person census files

HOUSEHOLD_OCC_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216_occ.Rdata"

load (HOUSEHOLD_OCC_RDATA)

household <- hbayarea1216_occ %>% mutate(           #Adjusted income to constant 2016$
  adjustedinc=HINCP*(ADJINC/1000000)
)

income_median <- wtd.quantile (household$adjustedinc, q=0.75, na.rm = FALSE, weight=household$WGTP) #Upper quartile

print(income_median)

