# ACS 2015 PUMS Group Quarters Total.R
# Analyze PUMS data for group quarters total population
# 2015 1-year PUMS data

# Import Library

suppressMessages(library(dplyr))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2015/pbayarea15.Rdata"
WD = "X:/Petrale/Output/"
setwd(WD)

load (PERSON_RDATA) 

gq15 <- pbayarea15 %>%
  select(PUMA,County_Name, PWGTP, RELP) %>%
  filter(RELP==17) %>%                                      # Non-institutional group quarters
  group_by(County_Name) %>%                                 # Sum by county
  summarize(gqsum15=sum(PWGTP))

gqbay15 <- pbayarea15 %>%
  select(PUMA,County_Name, PWGTP, RELP) %>%
  filter(RELP==17) %>%
  summarize(County_Name="Bay Area",gqsum15=sum(PWGTP))      # Sum for Bay Area

# Print to screen

print(gq15)
print(gqbay15)
