# ACS PUMS 2015-2019 .R
# Work through a few examples for using the R package Tidycensus to extract PUMS data
# Presentation for Census Data for Transportation Planning Conference in Reno, June 2022


# Include libraries for Census data extraction and working with tidyverse tools

library(tidyverse)
library(tidycensus)

# View PUMS data dictionary in R

View(pums_variables)

### Example script to look at California-to-Bay Area incommute

# Create vector of Bay Area PUMAs

baypuma    <- c("00101", "00102", "00103", "00104", "00105", "00106", "00107", "00108", "00109", "00110", 
                "01301", "01302", "01303", "01304", "01305", "01306", "01307", "01308", "01309", "04101", 
                "04102", "05500", "07501", "07502", "07503", "07504", "07505", "07506", "07507", "08101", 
                "08102", "08103", "08104", "08105", "08106", "08501", "08502", "08503", "08504", "08505", 
                "08506", "08507", "08508", "08509", "08510", "08511", "08512", "08513", "08514", "09501", 
                "09502", "09503", "09701", "09702", "09703")

# Create vector of Bay Area migration PUMAs, which in the Bay Area are coextensive with county geographies

baymigpuma <-  c("00100","01300","04100","05500","07500","08100","08500","09500","09700") 

# Script to extract 2019 california commuters by mode, 
# including recoded variable names and replicate weights for standard error calculation

bay_migration <- get_pums(
  variables = c("PUMA","MIGPUMA","MIGSP","RAC1P","HISP","COW","INDP","OCCP","HINCP","ADJINC","PINCP","PERNP"),
                survey = "acs5",
                state = "all",
                variables_filter = list(MIGPUMA=baymigpuma,MIGSP=006),
                year = 2019,
                ) 

%>%
  
# Still working within the same script, continued with the pipe ("%>%") operator
# Recode place-of-work PUMAs with Bay Area county names

   mutate(POW_NAME=recode(POWPUMA,
                        "00100"="Alameda",
                        "01300"="Contra Costa",
                        "04100"="Marin",
                        "05500"="Napa",
                        "07500"="San Francisco",
                        "08100"="San Mateo",
                        "08500"="Santa Clara",
                        "09500"="Solano",
                        "09700"="Sonoma")) %>% 
  
# Filter records to people residing in CA outside the Bay Area and commuting to Bay Area counties
# Filter out non-commuters ("bb")
# Move one variable (POW_NAME) in the dataset
# Select out unneeded variables to tidy up dataset
  
  filter(!(PUMA %in% baypuma), 
         POWPUMA %in% baypowpuma,
         POWSP_label=="California/CA",
         JWTRNS != "bb") %>% 
  relocate(POW_NAME,.before = PWGTP) %>% 
  select(-WGTP, -ST,-JWTRNS, -POWPUMA_label, -POWSP) 

# Create survey object from above dataframe 
# Summarize by POW PUMA and means of transportation to work
# Take a look at sum_database

bay_incommute_survey <- to_survey(bay_incommute)

sum_database <- bay_incommute_survey %>% 
  survey_count(POW_NAME, JWTRNS_label)

View (sum_database)

# Remove standard error variable
# Pivot from long to wide data format, using 0s to fill missing cells
# Rename variable for case consistency

CA_to_bay_incommute_final <- sum_database %>% select(-n_se) %>% 
  pivot_wider(names_from = JWTRNS_label,values_from = n,values_fill = 0) %>% 
  rename(County_of_Work=POW_NAME)

### Example script to summarize DVRPC active mode commuters by age and sex

# Get two vectors of PUMAs, one for the PA portion of DVRPC and one for the NJ portion
                                                
dvrpc_PA_pumas <- c(
  "03001","03002","03003","03004",                 # Bucks County
  "03401","03402","03403","03404",                 # Chester County
  "03301","03302","03303","03304",                 # Delaware County
  "03101","03102","03103","03104","03105","03106", # Montgomery County
  "03201","03202","03203","03204","03205","03206", # Philadelphia County
  "03207","03208","03209","03210","03211"          # Philadelphia County (cont.)
                 )

dvrpc_NJ_pumas <- c(
  "02001","02002","02003",                         # Burlington County
  "02101","02102","02103","02104",                 # Camden County
  "02201","02202",                                 # Gloucester County
  "02301","02302","02303"                          # Mercer County
                )
# Call PUMS API to get means of transportation, age, and sex variables
# Two calls required, one for each state's PUMAs
# Use variables_filter command to subset just bike/walk modes for JWTRNS variable
# Include recode = TRUE command to append a metadata labels column in dataset

dvrpc_PA <- get_pums(
  variables = c("JWTRNS","AGEP","SEX"),
  survey = "acs1",
  state = "PA",
  puma = dvrpc_PA_pumas,
  variables_filter = list(JWTRNS=09:10),           # Only people who bike to work
  recode = TRUE,
  year = 2019)

dvrpc_NJ <- get_pums(
  variables = c("JWTRNS","AGEP","SEX"),
  survey = "acs1",
  state = "NJ",
  puma = dvrpc_NJ_pumas,
  variables_filter = list(JWTRNS=09:10),              # Only people who bike/walk to work
  recode = TRUE,
  year = 2019)

# Concatenate ("rbind") two datasets and recode age variable to group bins
# Group records by age group and sex and summarize person weight
# Rename variable for case consistency

active_DVRPC <- rbind(dvrpc_PA,dvrpc_NJ) %>% 
  mutate(Age_Group=case_when(
    AGEP %in% 16:19                 ~ "16-19",
    AGEP %in% 20:24                 ~ "20-24",
    AGEP %in% 25:44                 ~ "25-44",
    AGEP %in% 45:54                 ~ "45-54",
    AGEP %in% 55:59                 ~ "55-59",
    AGEP %in% 60:64                 ~ "60-64",
    TRUE                            ~ "65+"
  )) %>% 
  group_by(Age_Group,SEX_label) %>% 
  summarize(Active_Commuters=sum(PWGTP)) %>% 
  rename(Sex_Label=SEX_label)

# Plot DVRPC active modes by age and sex using GGPLOT2 package
   
ggplot(data=active_DVRPC, aes(x=Age_Group, y=Active_Commuters, fill=Sex_Label)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=scales::comma(Active_Commuters)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+theme_minimal()+
  ggtitle("ACS 2019 PUMS: DVRPC Walk/Bike to Work by Age and Gender")+
  theme(plot.title = element_text(hjust = 0.5))
  
  
