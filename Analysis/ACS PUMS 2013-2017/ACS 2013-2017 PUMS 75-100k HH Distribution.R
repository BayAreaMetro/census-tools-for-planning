# ACS 2013-2017 PUMS 75-100k HH Distribution.R

# Analyze 2013-2017 Bay Area PUMS data for share of households above and below $91,538 within $75-100k

suppressMessages(library(dplyr))

# Input person census files

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/hbayarea1317.Rdata"

load (HOUSEHOLD_RDATA)

# Keep only needed variables, find share for 2017$

household_17 <- hbayarea1317 %>%
  select(PUMA, HINCP, ADJINC, WGTP) %>%
  mutate(adjustedinc=HINCP*(ADJINC/1000000)) %>%
  filter(adjustedinc>=75000 & adjustedinc<99999) %>%
  mutate(
    above91538=if_else(adjustedinc>=91538,WGTP,0L),
    below91538=if_else(adjustedinc<91538,WGTP,0L)
  )

sum_household_17 <- household_17 %>%
  summarize(above=sum(above91538), below=sum(below91538), total=sum(WGTP)) %>% 
  mutate(shareabove=above/total)

print(sum_household_17$shareabove)
