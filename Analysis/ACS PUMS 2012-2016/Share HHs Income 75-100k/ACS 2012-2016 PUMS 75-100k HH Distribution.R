# ACS 2012-2016 PUMS 75-100k HH Distribution.R

# Analyze 2012-2016 Bay Area PUMS data for share of households above and below $88,681 within $75-100k

suppressMessages(library(dplyr))
SUMMARY_OUT="M:/Data/Requests/Lisa Zorn/Median Income 75-100k/"  # work directory

# Input person census files

HOUSEHOLD_RDATA = "M:/Data/Census/PUMS/PUMS 2012-16/hbayarea1216_occ.Rdata"

load (HOUSEHOLD_RDATA)

# Keep only needed variables

household <- hbayarea1216_occ %>%
  select(PUMA, HINCP, ADJINC, WGTP) %>%
  mutate(adjustedinc=HINCP*(ADJINC/1000000)) %>%
  filter(adjustedinc>=75000 & adjustedinc<99999) %>%
  mutate(
    above88681=if_else(adjustedinc>=88681,WGTP,0L),
    below88681=if_else(adjustedinc<88681,WGTP,0L)
    )

sum_household <- household %>%
  summarize(above=sum(above88681), below=sum(below88681), total=sum(WGTP)) %>% 
  mutate(shareabove=above/total)

print(sum_household$shareabove)
