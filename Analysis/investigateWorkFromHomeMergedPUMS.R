
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
  NWAB,     # Temporary absence from work (UNEDITED),
  AGEP,     # Age
  SCHG,     # Grade level attending
  WKHP,     # Usual hours worked per week past 12 months
  WKW,      # Weeks worked during past 12 months  
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
  NWAB,     # Temporary absence from work (UNEDITED)
  AGEP,     # Age
  SCHG,     # Grade level attending
  WKHP,     # Usual hours worked per week past 12 months
  WKWN,     # Weeks worked during past 12 months  
)

# Definitions are the same except:
# SERIALNO is number in PUMS2013-2017 but character in PUMS 2021
# JWTR == JWTRNS
# ADJINCP*PINCP = 2017 dollars for PUMS 2013-2017
# ADJINCP*PINCP = 2021 dollars for PUMS 2021
# WKW is a set of codes, WKWN is number
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
  PINCP_2000dollars = PINCP_2021dollars*DOLLARS_2021_TO_2000,
  # recode WKWN to WKW categories
  WKW = if_else(WKWN >= 50 & WKWN <= 52, 1, NA, NA),
  WKW = if_else(WKWN >= 48 & WKWN <= 49, 2, WKW, NA),
  WKW = if_else(WKWN >= 40 & WKWN <= 47, 3, WKW, NA),
  WKW = if_else(WKWN >= 27 & WKWN <= 39, 4, WKW, NA),
  WKW = if_else(WKWN >= 14 & WKWN <= 26, 5, WKW, NA),
  WKW = if_else(WKWN >=  1 & WKWN <  14, 6, WKW, NA),
) %>% select(-PINCP, -ADJINC, -PINCP_2021dollars, -WKWN)

pbayarea_combined <- rbind(pbayarea1317, pbayarea21)

# student_status: 
#   1 is pre-school through grade 12 student, 
#   2 is university/professional school student
#   3 is non-student
pbayarea_combined <- mutate(
  pbayarea_combined,
  student_status = 3,
  student_status = if_else (SCHG >= 1 & SCHG <= 14, 1, student_status, 3), # pre 12
  student_status = if_else (SCHG >=15 & SCHG <= 16, 2, student_status, 3), # college
) %>% select(-SCHG)

# set employment status based on employment status recode, weeks worked per year, and hours worked per week
# employ_status:
#  1 is full-time worker
#  2 is part-time worker
#  3 is not in the labor force
#  4 is student under 16
pbayarea_combined <- mutate(
  pbayarea_combined,
  employed = if_else( ESR %in% c(1,2,4,5), 1, 0, 0),
  # default to part-time for employed, or not in the labor force
  employ_status = if_else( employed==1, 2, 3, NA),
  # convert some to full-time-worker
  employ_status = if_else((employed == 1)&(WKW %in% c(1,2,3,4))&(WKHP>=35), 1, employ_status, NA),
  employ_status = if_else(AGEP < 16, 4, employ_status, NA)
)

# code person_type consistent with 
# populationsim/bayarea/create_seed_population.py
#  1 is full-time worker
#  2 is part-time worker
#  3 is college student
#  4 is non-working adult 
#  5 is retired
#  6 is driving-age student
#  7 is non-driving age student
#  8 is child too young for school
pbayarea_combined <- mutate(
  pbayarea_combined,
  person_type = 5, # non-working senior
  person_type = ifelse(AGEP < 65, 4, person_type),         # non-working adult
  person_type = ifelse(employ_status==1, 1, person_type),  # full-time worker
  person_type = ifelse(employ_status==2, 2, person_type),  # part-time worker
  person_type = ifelse((student_status==2)|((AGEP>=20)&(student_status==1)), 3, person_type), # college student
  person_type = ifelse(student_status==1, 6, person_type), # driving-age student
  person_type = ifelse(AGEP <=15, 7, person_type),         # non-driving under 16
  person_type = ifelse((AGEP < 6)&(student_status==3), 8, person_type), # pre-school
)

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
