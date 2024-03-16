library(jsonlite)
library(tidyverse)
library(tidycensus)

temp1 <- fromJSON("https://api.census.gov/data/2020/dec/dhc?get=NAME,P1_001N,H8_001N,H3_001N,H3_002N,H3_003N&for=place%20(or%20part):*&in=state:06%20county:001,013,041,055,075,081,085,095,097")

bayarea1 <- as.data.frame(temp1)

bayarea2 <- bayarea1 %>% 
  filter(!V1=="NAME") %>%
  mutate_at(c("V2","V3","V4","V5","V6"), as.numeric) %>% 
  rename(place_name = V1,
         totpop_2020     = V2,   # P1_001N
         hhpop_2020      = V3,   # H8_001N
         total_du_2020   = V4,   # H3_001N
         occ_du_2020     = V5,   # H3_002N
         vac_du_2020     = V6,   # H3_003N
         state_fips      = V7,
         county_fips     = V8,
         place_fips      = V9) %>% 
  unite(GEOID,c("state_fips","place_fips"),sep="", remove=FALSE)
