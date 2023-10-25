
library(dplyr)
library(foreign)
library(stringr)
PUMS_DIR = "M:/Data/Census/PUMS"
PUMA10_FILE = "M:/Data/Census/Geography/tl_2010_06_puma10/tl_2010_06_puma10.dbf"
PUMA20_FILE = "M:/Data/Census/Geography/tl_2020_06_puma20/tl_2020_06_puma20.dbf"


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
  NAICSP,   # NAICS Industry code based on 2012 NAICS codes
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
  NAICSP,   # North American Industry Classification System (NAICS) recode for 2018 and later based on 2017 NAICS codes
)

# Read PUMS 2022 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2022", "pbayarea22.Rdata"))
pbayarea22 = select(
  pbayarea22,
  SERIALNO, # Housing unit/GQ person serial number
  SPORDER,  # Person number
  PWGTP,    # Person weight
  PUMA,     # Public use microdata area code (PUMA) based on 2020 Census definition
  ST,       # State code based on 2020 Census definitions
  POWPUMA,  # Place of work PUMA based on 2020 Census
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
  NAICSP,   # North American Industry Classification System (NAICS) recode for 2018 and later based on 2017 NAICS codes
)


# Definitions are the same except:
# SERIALNO is number in PUMS2013-2017 but character in PUMS 2021 and 2022
# JWTR == JWTRNS
# ADJINCP*PINCP = 2017 dollars for PUMS 2013-2017
# ADJINCP*PINCP = 2021 dollars for PUMS 2021
# ADJINCP*PINCP = 2022 dollars for PUMS 2022
# WKW is a set of codes, WKWN is number
# https://github.com/BayAreaMetro/modeling-website/wiki/InflationAssumptions
# PUMS2013-2017 uses 2010 Census geometries but PUMS2022 uses 2020 Census geometries
DOLLARS_2017_TO_2000 = 1.0/1.53
DOLLARS_2021_TO_2000 = 1.0/1.72
DOLLARS_2022_TO_2000 = 1.0/1.81
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
  WKW = if_else(WKWN >= 50 & WKWN <= 52, 1, NA_real_, NA_real_),
  WKW = if_else(WKWN >= 48 & WKWN <= 49, 2, WKW, NA_real_),
  WKW = if_else(WKWN >= 40 & WKWN <= 47, 3, WKW, NA_real_),
  WKW = if_else(WKWN >= 27 & WKWN <= 39, 4, WKW, NA_real_),
  WKW = if_else(WKWN >= 14 & WKWN <= 26, 5, WKW, NA_real_),
  WKW = if_else(WKWN >=  1 & WKWN <  14, 6, WKW, NA_real_),
) %>% select(-PINCP, -ADJINC, -PINCP_2021dollars, -WKWN)

pbayarea22 <- mutate(
  pbayarea22,
  source = "PUMS2022",
  PINCP_2022dollars = PINCP*(ADJINC/ONE_MILLION),
  PINCP_2000dollars = PINCP_2022dollars*DOLLARS_2022_TO_2000,
  # recode WKWN to WKW categories
  WKW = if_else(WKWN >= 50 & WKWN <= 52, 1, NA_real_, NA_real_),
  WKW = if_else(WKWN >= 48 & WKWN <= 49, 2, WKW, NA_real_),
  WKW = if_else(WKWN >= 40 & WKWN <= 47, 3, WKW, NA_real_),
  WKW = if_else(WKWN >= 27 & WKWN <= 39, 4, WKW, NA_real_),
  WKW = if_else(WKWN >= 14 & WKWN <= 26, 5, WKW, NA_real_),
  WKW = if_else(WKWN >=  1 & WKWN <  14, 6, WKW, NA_real_),
) %>% select(-PINCP, -ADJINC, -PINCP_2022dollars, -WKWN)
  # also, conver PUMA and ST columns to integer to be consistent with other datasets
pbayarea22 <- transform(
  pbayarea22,
  PUMA = as.integer(PUMA),
  ST   = as.integer(ST)
)

pbayarea_combined <- rbind(pbayarea1317, pbayarea21, pbayarea22)

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
  employ_status = if_else( employed==1, 2, 3, NA_real_),
  # convert some to full-time-worker
  employ_status = if_else((employed == 1)&(WKW %in% c(1,2,3,4))&(WKHP>=35), 1, employ_status, NA_real_),
  employ_status = if_else(AGEP < 16, 4, employ_status, NA_real_)
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

# code NAICSP to empsix categories
# See petrale\applications\travel_model_lu_inputs\2015\Employment\NAICS_to_EMPSIX.xlsx
pbayarea_combined$NAICSP <- as.character(pbayarea_combined$NAICSP)
pbayarea_combined <- mutate(
  pbayarea_combined,
  empsix = 'Unknown',
  empsix = ifelse(startsWith(NAICSP,"11"), "Agriculture & Natural Resources", empsix),
  empsix = ifelse(startsWith(NAICSP,"21"), "Agriculture & Natural Resources", empsix),
  empsix = ifelse(startsWith(NAICSP,"22"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"23"), "Other", empsix),
  empsix = ifelse(startsWith(NAICSP,"31"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"32"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"33"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"42"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"44"), "Retail ", empsix),
  empsix = ifelse(startsWith(NAICSP,"45"), "Retail ", empsix),
  empsix = ifelse(startsWith(NAICSP,"48"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"49"), "Manufacturing, Wholesale & Transportation", empsix),
  empsix = ifelse(startsWith(NAICSP,"51"), "Other", empsix),
  empsix = ifelse(startsWith(NAICSP,"52"), "Financial & Professional Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"53"), "Financial & Professional Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"54"), "Financial & Professional Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"55"), "Financial & Professional Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"56"), "Financial & Professional Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"61"), "Health, Educational & Recreational Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"62"), "Health, Educational & Recreational Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"71"), "Health, Educational & Recreational Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"72"), "Health, Educational & Recreational Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"81"), "Health, Educational & Recreational Services", empsix),
  empsix = ifelse(startsWith(NAICSP,"92"), "Other", empsix),
)
NAICS_empsix <- group_by(pbayarea_combined, NAICSP, empsix) %>% summarise(total_count=n(), .groups='drop')
pbayarea_combined <- select(pbayarea_combined, -NAICSP)
pbayarea_combined$empsix <- as.factor(pbayarea_combined$empsix)
print(NAICS_empsix)

# don't drop folks without POWPUMA -- universe == all residents

# country/state = 3 characters
# PUMA = 5 characters

# Add PUMA name and county
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMACE = as.factor(sprintf('%03d%05d', ST, PUMA)),
  POWPUMACE = as.factor(sprintf('%03d%05d', POWSP, POWPUMA)),
  POWPUMACE = substr(POWPUMACE, 0, 6) # POWPUMA is only county
)

# label PUMA geo version
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMA_version = if_else(pbayarea_combined$source %in% list('PUMS2013-2017', 'PUMS2021'), 'geo2010', 'geo2020'))

# check col types, make sure PUMACE or POWPUMACE are character cols
print(str(pbayarea_combined))
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMACE_CH = as.character(PUMACE))
pbayarea_combined <- mutate(
  pbayarea_combined,
  POWPUMACE_CH = as.character(POWPUMACE))

# create diff columns for joins (later steps)
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMACE20 = 'n/a',
  PUMACE20 = ifelse(PUMA_version == 'geo2020', PUMACE_CH, PUMACE20))
pbayarea_combined <- mutate(
  pbayarea_combined,
  PUMACE10 = 'n/a',
  PUMACE10 = ifelse(PUMA_version == 'geo2010', PUMACE_CH, PUMACE10))

pbayarea_combined <- mutate(
  pbayarea_combined,
  POWPUMACE20 = 'n/a',
  POWPUMACE20 = ifelse(PUMA_version == 'geo2020', POWPUMACE_CH, POWPUMACE20))
pbayarea_combined <- mutate(
  pbayarea_combined,
  POWPUMACE10 = 'n/a',
  POWPUMACE10 = ifelse(PUMA_version == 'geo2010', POWPUMACE_CH, POWPUMACE10))

# read PUMA to county lookup
PUMA_COUNTY10 <- read.dbf(PUMA10_FILE) %>% select(PUMACE10, GEOID10, NAMELSAD10)
PUMA_COUNTY10 <- mutate(
  PUMA_COUNTY10, 
  GEOID10 = paste0("0",GEOID10), # county + PUMA but PUMS has a leading country code char
  COUNTY=substr(GEOID10, 0, 6),
  # COUNTYNAME = str_extract(NAMELSAD10, regex("(.*) (County|Counties)"), group=1) # this line errors for me: "unused argument (group=1)
  COUNTYNAME = str_extract(NAMELSAD10, regex("(.*) (County|Counties)"))
)
PUMA_COUNTY20 <- read.dbf(PUMA20_FILE) %>% select(PUMACE20, GEOID20, NAMELSAD20)
PUMA_COUNTY20 <- mutate(
  PUMA_COUNTY20, 
  GEOID20 = paste0("0",GEOID20), # county + PUMA but PUMS has a leading country code char
  COUNTY=substr(GEOID20, 0, 6),
  # COUNTYNAME = str_extract(NAMELSAD20, regex("(.*) (County|Counties)"), group=1) # this line errors for me: "unused argument (group=1)
  COUNTYNAME = str_extract(NAMELSAD20, regex("(.*) (County|Counties)"))
)

# make sure the new fiels have character type, not Factor type
print(str(PUMA_COUNTY10))
PUMA_COUNTY10[] <- lapply(PUMA_COUNTY10, as.character)
print(str(PUMA_COUNTY10))
print(str(PUMA_COUNTY20))
PUMA_COUNTY20[] <- lapply(PUMA_COUNTY20, as.character)
print(str(PUMA_COUNTY20))

# POW is really just COUNTY, not PUMA
# COUNTY is a 6-char number string
# COUNTYNAME is the name based on the PUMA file
COUNTY_NO_PUMA10 <- select(PUMA_COUNTY10, COUNTY, COUNTYNAME) %>%
  arrange(COUNTY) %>%
  distinct(COUNTY, .keep_all=TRUE)
COUNTY_NO_PUMA20 <- select(PUMA_COUNTY20, COUNTY, COUNTYNAME) %>%
  arrange(COUNTY) %>%
  distinct(COUNTY, .keep_all=TRUE)

# join for PUMA of residence
pbayarea_combined <- left_join(
  pbayarea_combined,
  select(PUMA_COUNTY10, -PUMACE10) %>% rename(PUMACE10 = GEOID10,
                                              COUNTY10 = COUNTY,
                                              COUNTYNAME10 = COUNTYNAME)
)
pbayarea_combined <- left_join(
  pbayarea_combined,
  select(PUMA_COUNTY20, -PUMACE20) %>% rename(PUMACE20 = GEOID20,
                                              COUNTY20 = COUNTY,
                                              COUNTYNAME20 = COUNTYNAME)
)

# join for PUMA (really county) of workplace
pbayarea_combined <- left_join(
  pbayarea_combined,
  rename(COUNTY_NO_PUMA10, 
         POWPUMACE10 = COUNTY,
         POWCOUNTYNAME10 = COUNTYNAME)
)
pbayarea_combined <- left_join(
  pbayarea_combined,
  rename(COUNTY_NO_PUMA20, 
         POWPUMACE20 = COUNTY,
         POWCOUNTYNAME20 = COUNTYNAME)
)

# consolidate into one COUNTY and one COUNTYNAME field
pbayarea_combined <- mutate(
  pbayarea_combined,
  NAMELSAD = ifelse(PUMA_version == 'geo2020', NAMELSAD20, NAMELSAD10))
pbayarea_combined <- mutate(
  pbayarea_combined,
  COUNTY = ifelse(PUMA_version == 'geo2020', COUNTY20, COUNTY10))
pbayarea_combined <- mutate(
  pbayarea_combined,
  COUNTYNAME = ifelse(PUMA_version == 'geo2020', COUNTYNAME20, COUNTYNAME10))
pbayarea_combined <- mutate(
  pbayarea_combined,
  POWCOUNTYNAME = ifelse(PUMA_version == 'geo2020', POWCOUNTYNAME20, POWCOUNTYNAME10))

pbayarea_combined <- select(
  pbayarea_combined, -c(NAMELSAD10,      NAMELSAD20,
                        PUMACE10,        PUMACE20,
                        POWPUMACE10,     POWPUMACE20,
                        COUNTY10,        COUNTY20,
                        COUNTYNAME10,    COUNTYNAME20,
                        POWCOUNTYNAME10, POWCOUNTYNAME20))

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

# Read PUMS 2022 & keep subset of variables
load(file.path(PUMS_DIR, "PUMS 2022", "hbayarea22.Rdata"))
hbayarea22 = select(
  hbayarea22,
  SERIALNO, # Housing unit/GQ person serial number
  WGTP,     # Housing weight
  PUMA,     # Public use microdata area code (PUMA) based on 2020 Census definition
  ST,       # State code based on 2020 Census definitions
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
# also, conver PUMA and ST columns to integer to be consistent with other datasets
hbayarea22 <- transform(
  hbayarea22,
  PUMA = as.integer(PUMA),
  ST   = as.integer(ST)
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

hbayarea22 <- mutate(
  hbayarea22,
  source = "PUMS2022",
  HINCP_2022dollars = HINCP*(ADJINC/ONE_MILLION),
  HINCP_2000dollars = HINCP_2022dollars*DOLLARS_2022_TO_2000
) %>% select(-HINCP, -ADJINC, -HINCP_2022dollars)

hbayarea_combined <- rbind(hbayarea1317, hbayarea21, hbayarea22)

# Add PUMA name
hbayarea_combined <- mutate(
  hbayarea_combined,
  PUMACE = as.factor(sprintf('%03d%05d', ST, PUMA)),
  PUMACE = as.character(PUMACE)
)

# label PUMA geo version
hbayarea_combined <- mutate(
  hbayarea_combined,
  PUMA_version = if_else(hbayarea_combined$source %in% list('PUMS2013-2017', 'PUMS2021'), 'geo2010', 'geo2020'))

# create diff columns for joins (later steps)
hbayarea_combined <- mutate(
  hbayarea_combined,
  PUMACE20 = 'n/a',
  PUMACE20 = ifelse(PUMA_version == 'geo2020', PUMACE, PUMACE20))
hbayarea_combined <- mutate(
  hbayarea_combined,
  PUMACE10 = 'n/a',
  PUMACE10 = ifelse(PUMA_version == 'geo2010', PUMACE, PUMACE10))

# join for PUMA of residence
hbayarea_combined <- left_join(
  hbayarea_combined,
  select(PUMA_COUNTY10, -PUMACE10) %>% rename(PUMACE10 = GEOID10,
                                              COUNTY10 = COUNTY,
                                              COUNTYNAME10 = COUNTYNAME)
)
hbayarea_combined <- left_join(
  hbayarea_combined,
  select(PUMA_COUNTY20, -PUMACE20) %>% rename(PUMACE20 = GEOID20,
                                              COUNTY20 = COUNTY,
                                              COUNTYNAME20 = COUNTYNAME)
)

# consolidate into one COUNTY and one COUNTYNAME field
hbayarea_combined <- mutate(
  hbayarea_combined,
  NAMELSAD = ifelse(PUMA_version == 'geo2020', NAMELSAD20, NAMELSAD10))
hbayarea_combined <- mutate(
  hbayarea_combined,
  COUNTY = ifelse(PUMA_version == 'geo2020', COUNTY20, COUNTY10))
hbayarea_combined <- mutate(
  hbayarea_combined,
  COUNTYNAME = ifelse(PUMA_version == 'geo2020', COUNTYNAME20, COUNTYNAME10))

hbayarea_combined <- select(
  hbayarea_combined, -c(PUMACE10,        PUMACE20,
                        NAMELSAD10,      NAMELSAD20,
                        COUNTY10,        COUNTY20,
                        COUNTYNAME10,    COUNTYNAME20))

# write it
save(hbayarea_combined,
     file = file.path(PUMS_DIR, "WorkFromHomeInvestigation", "hbayarea_combined.Rdata"))
