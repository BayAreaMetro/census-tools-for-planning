# ACS 2017-2021 PUMS 150-200k HH Distribution.R

# Analyze 2017-2021 Bay Area PUMS data for share of households above and below $171,876 within $150-200k

suppressMessages(library(dplyr))

# Input person census files

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/hbayarea1721.Rdata"

load (HOUSEHOLD_RDATA)

# Keep only needed variables, find share for 2021$

household_21 <- hbayarea1721 %>%
  select(PUMA, HINCP, ADJINC, WGTP) %>%
  mutate(adjustedinc=HINCP*(ADJINC/1000000)) %>%
  filter(adjustedinc>=150000 & adjustedinc<199999) %>%
  mutate(
    above171876=if_else(adjustedinc>=171876,WGTP,0L),
    below171876=if_else(adjustedinc<171876,WGTP,0L)
  )

sum_household_21 <- household_21 %>%
  summarize(above=sum(above171876), below=sum(below171876), total=sum(WGTP)) %>% 
  mutate(shareabove=above/total)

print(sum_household_21$shareabove)
