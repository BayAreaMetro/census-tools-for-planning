# RMWG_PUMS_Code_0822.R
# Work through a few examples for using the R package Tidycensus to extract PUMS data
# Presentation for Bay Area Regional Modeling Working Group on August 3, 2022


# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# View PUMS data dictionary in R

#View(pums_variables)

### Example script to look at California-to-Bay Area incommute

# Create vector of Bay Area PUMAs

baypuma    <- c("00101", "00102", "00103", "00104", "00105", "00106", "00107", "00108", "00109", "00110", 
                "01301", "01302", "01303", "01304", "01305", "01306", "01307", "01308", "01309", "04101", 
                "04102", "05500", "07501", "07502", "07503", "07504", "07505", "07506", "07507", "08101", 
                "08102", "08103", "08104", "08105", "08106", "08501", "08502", "08503", "08504", "08505", 
                "08506", "08507", "08508", "08509", "08510", "08511", "08512", "08513", "08514", "09501", 
                "09502", "09503", "09701", "09702", "09703")

# Create vector of Bay Area place-of-work PUMAs(POWPUMA), which in the Bay Area are coextensive with county geographies

baypowpuma <-  c("00100","01300","04100","05500","07500","08100","08500","09500","09700") 

# Script to extract 2019 california commuters by mode, 
# including recoded variable names and replicate weights for standard error calculation

bay_commuters <- get_pums(
  variables = c("POWPUMA", "POWSP", "PWGTP"),
                survey = "acs1",
                state = "CA",
                puma = baypuma,
                variables_filter = list(POWPUMA=baypowpuma,POWSP=006),  # Only people who work in the Bay Area
                year = 2019,
                recode = TRUE
                ) 

bay_commuters <- bay_commuters %>% 
  mutate(
    home_fips=str_sub(PUMA,1,3),
    home_county=recode(home_fips,
                        "001"                 = "Alameda",
                        "013"                 = "Contra Costa",
                        "041"                 = "Marin",
                        "055"                 = "Napa",
                        "075"                 = "San Francisco",
                        "081"                 = "San Mateo",
                        "085"                 = "Santa Clara",
                        "095"                 = "Solano",
                        "097"                 = "Sonoma"
  ),
  work_county=recode(POWPUMA,
                        "00100"               ="Alameda",
                        "01300"               ="Contra Costa",
                        "04100"               ="Marin",
                        "05500"               ="Napa",
                        "07500"               ="San Francisco",
                        "08100"               ="San Mateo",
                        "08500"               ="Santa Clara",
                        "09500"               ="Solano",
                        "09700"               ="Sonoma")) 

county_to_county <- bay_commuters %>% 
  group_by(home_county,work_county) %>% 
  summarize(tot_commuters=sum(PWGTP)) %>% 
  pivot_wider(names_from = work_county,values_from = tot_commuters,values_fill = 0) 

sf_bay_incommute <- bay_commuters %>% 
  filter(home_county != "San Francisco",work_county=="San Francisco") %>% 
  mutate(mode=case_when(
    JWTRNS=="01" & JWRIP==1                            ~ "Drive alone",
    JWTRNS=="01" & JWRIP>=2                            ~ "Carpool",
    JWTRNS %in% c("02","03","04","05","06")            ~ "Transit",
    JWTRNS %in% c("07","08")                           ~ "Other",
    JWTRNS=="09"                                       ~ "Bicycle",
    JWTRNS=="10"                                       ~ "Walk",
    JWTRNS=="12"                                       ~ "Other"
  )) %>% 
  group_by(mode) %>% 
  summarize(sf_incommuters=sum(PWGTP)) 

ggplot(data=sf_bay_incommute, aes(x=mode,y=sf_incommuters))+  
  geom_bar(stat="identity",width=0.5)     


           


