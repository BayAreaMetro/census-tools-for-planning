
library(dplyr)
library(foreign)
library(stringr)
PUMS_DIR = "M:/Data/Census/PUMS"
PUMA10_FILE = "M:/Data/Census/Geography/tl_2010_06_puma10/tl_2010_06_puma10.dbf"

# Read PUMS 2013-2017 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2013-17", "pbayarea1317.Rdata"))
pbayarea1317 = select(
  pbayarea1317,
  SERIALNO, # Housing unit/GQ person serial number
  SPORDER,  # Person number
  PWGTP,    # Person weight
  PUMA,     # Public use microdata area code (PUMA) based on 2010 Census definition
  ST,       # State code based on 2010 Census definitions
  POWPUMA,  # Place of work PUMA based on 2010 Census
  POWSP,    # Place of work - State or foreign country recode
  JWMNP,    # Travel time to work
  JWTR,     # Means of transportation to work
  PINCP,    # Total person's income (signed, use ADJINC to adjust to constant dollars)
  ADJINC,   # Adjustment factor for income and earnings dollar amounts
  ESR,      # Employment status recode
  NWAB,     # Temporary absence from work (UNEDITED
)

# Read PUMS 2021 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2021", "pbayarea21.Rdata"))
pbayarea21 = select(
  pbayarea21,
  SERIALNO, # Housing unit/GQ person serial number
  SPORDER,  # Person number
  PWGTP,    # Person weight
  PUMA,     # Public use microdata area code (PUMA) based on 2010 Census definition
  ST,       # State code based on 2010 Census definitions
  POWPUMA,  # Place of work PUMA based on 2010 Census
  POWSP,    # Place of work - State or foreign country recode
  JWMNP,    # Travel time to work
  JWTRNS,   # Means of transportation to work
  PINCP,    # Total person's income (signed, use ADJINC to adjust to constant dollars)
  ADJINC,   # Adjustment factor for income and earnings dollar amounts
  ESR,      # Employment status recode
  NWAB,     # Temporary absence from work (UNEDITED
)

# Definitions are the same except:
# SERIALNO is number in PUMS2013-2017 but character in PUMS 2021
# JWTR == JWTRNS
# ADJINCP*PINCP = 2017 dollars for PUMS 2013-2017
# ADJINCP*PINCP = 2021 dollars for PUMS 2021

# https://github.com/BayAreaMetro/modeling-website/wiki/InflationAssumptions
DOLLARS_2017_TO_2000 = 1.0/1.53
DOLLARS_2021_TO_2000 = 1.0/1.72
ONE_MILLION = 1000000

pbayarea1317 <- rename(pbayarea1317, JWTRNS=JWTR)
pbayarea1317 <- mutate(
  pbayarea1317, 
  source = "PUMS2013-2017",
  SERIALNO = as.character(SERIALNO),
  PINCP_2017dollars = PINCP*(ADJINC/ONE_MILLION),
  PINCP_2000dollars = PINCP_2017dollars*DOLLARS_2017_TO_2000
) %>% select(-PINCP, -ADJINC, -PINCP_2017dollars)

pbayarea21 <- mutate(
  pbayarea21,
  source = "PUMS2021",
  PINCP_2021dollars = PINCP*(ADJINC/ONE_MILLION),
  PINCP_2000dollars = PINCP_2021dollars*DOLLARS_2021_TO_2000
) %>% select(-PINCP, -ADJINC, -PINCP_2021dollars)

pbayarea_combined <- rbind(pbayarea1317, pbayarea21)

# don't drop folks without POWPUMA -- universe == all residents

# country/state = 3 characters
# PUMA = 5 characters

# Add PUMA name and county
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMACE10 = as.factor(sprintf('%03d%05d', ST, PUMA)),
  POWPUMACE10 = as.factor(sprintf('%03d%05d', POWSP, POWPUMA)),
  POWPUMACE10 = substr(POWPUMACE10, 0, 6) # POWPUMA is only county
)

# read PUMA to county lookup
PUMA_COUNTY <- read.dbf(PUMA10_FILE) %>% select(PUMACE10, GEOID10, NAMELSAD10)
PUMA_COUNTY <- mutate(
  PUMA_COUNTY, 
  GEOID10 = paste0("0",GEOID10), # county + PUMA but PUMS has a leading country code char
  COUNTY=substr(GEOID10, 0, 6),
  COUNTYNAME = str_extract(NAMELSAD10, regex("(.*) (County|Counties)"), group=1)
)
# POW is really just COUNTY, not PUMA
# COUNTY is a 6-char number string
# COUNTYNAME is the name based on the PUMA file
COUNTY_NO_PUMA <- select(PUMA_COUNTY, COUNTY, COUNTYNAME) %>%
  arrange(COUNTY) %>%
  distinct(COUNTY, .keep_all=TRUE)

# join for PUMA of residence
pbayarea_combined <- left_join(
  pbayarea_combined,
  select(PUMA_COUNTY, -PUMACE10) %>% rename(PUMACE10 = GEOID10)
)
# join for PUMA (really county) of workplace
pbayarea_combined <- left_join(
  pbayarea_combined,
  rename(COUNTY_NO_PUMA, 
         POWPUMACE10 = COUNTY,
         POWCOUNTYNAME = COUNTYNAME)
)

# write it
save(pbayarea_combined,
     file = file.path(PUMS_DIR, "WorkFromHomeInvestigation", "pbayarea_combined.Rdata"))

# Households now
# Read PUMS 2013-2017 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2013-17", "hbayarea1317.Rdata"))
hbayarea1317 = select(
  hbayarea1317,
  SERIALNO, # Housing unit/GQ person serial number
  WGTP,     # Housing weight
  PUMA,     # Public use microdata area code (PUMA) based on 2010 Census definition
  ST,       # State code based on 2010 Census definitions
  NP,       # Number of person records associated with this housing record
  TYPE,     # Type of unit
  BLD,      # Units in structure
  HHT,      # Household/family type (Note: there's also HHT2)
  HINCP,    # Household income (past 12 months, use ADJINC to adjust HINCP to constant dollars))
  ADJINC,   # Adjustment factor for income and earnings dollar amounts
  HUPAC,    # HH presence and age of children
  NPF,      # Number of persons in family (unweighted)
  TEN,      # Tenure
  VEH,      # Vehicles (1 ton or less) available
)

# Read PUMS 2021 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2021", "hbayarea21.Rdata"))
hbayarea21 = select(
  hbayarea21,
  SERIALNO, # Housing unit/GQ person serial number
  WGTP,     # Housing weight
  PUMA,     # Public use microdata area code (PUMA) based on 2010 Census definition
  ST,       # State code based on 2010 Census definitions
  NP,       # Number of person records associated with this housing record
  TYPEHUGQ, # Type of unit
  BLD,      # Units in structure
  HHT,      # Household/family type (Note: there's also HHT2)
  HINCP,    # Household income (past 12 months, use ADJINC to adjust HINCP to constant dollars))
  ADJINC,   # Adjustment factor for income and earnings dollar amounts
  HUPAC,    # HH presence and age of children
  NPF,      # Number of persons in family (unweighted)
  TEN,      # Tenure
  VEH,      # Vehicles (1 ton or less) available
)

hbayarea1317 <- rename(hbayarea1317, TYPEHUGQ=TYPE)
hbayarea1317 <- mutate(
  hbayarea1317, 
  source = "PUMS2013-2017",
  SERIALNO = as.character(SERIALNO),
  HINCP_2017dollars = HINCP*(ADJINC/ONE_MILLION),
  HINCP_2000dollars = HINCP_2017dollars*DOLLARS_2017_TO_2000
) %>% select(-HINCP, -ADJINC, -HINCP_2017dollars)

hbayarea21 <- mutate(
  hbayarea21,
  source = "PUMS2021",
  HINCP_2021dollars = HINCP*(ADJINC/ONE_MILLION),
  HINCP_2000dollars = HINCP_2021dollars*DOLLARS_2021_TO_2000
) %>% select(-HINCP, -ADJINC, -HINCP_2021dollars)

hbayarea_combined <- rbind(hbayarea1317, hbayarea21)

# Add PUMA name
hbayarea_combined <- mutate(
  hbayarea_combined,
  PUMACE10 = as.factor(sprintf('%03d%05d', ST, PUMA)),
)

# join for PUMA of residence
hbayarea_combined <- left_join(
  hbayarea_combined,
  select(PUMA_COUNTY, -PUMACE10) %>% rename(PUMACE10 = GEOID10)
)

# write it
save(hbayarea_combined,
     file = file.path(PUMS_DIR, "WorkFromHomeInvestigation", "hbayarea_combined.Rdata"))
