# Import Libraries
library(dplyr)
library(tidycensus)
library(writexl)

censuskey <- readLines("C:/Users/jhalpern/Documents/censuskey.txt")

census_api_key(censuskey, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

ACS_year=2018
ACS_product="acs5"

#geographies
baycounties=c("01","13","41","55","75","81","85","95","97")
statenumber="06"
metros = c("37980","47900","26420","33100","31080","16980","19100","35620","12060") # if 2014 or after metro data
source_black="B18101B_ACS18_5YR"

v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
v18_acs1 <- load_variables(2018, "acs1", cache = TRUE)
# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."
#Because of the way race/ethnicity interact in the census. First all races (including White Alone) was first calculated
#Following that a separate table was made to separate non-Hispanic whites and Hispanic/Latinos
ACS_variables_race <- c(Wht_Under18_Dis = "B18101A_003",
                        Wht_Under18_NoDis = "B18101A_004",
                        Wht_18to64_Dis = "B18101A_006",
                        Wht_18to64_NoDis = "B18101A_007",
                        Wht_65Over_Dis = "B18101A_009",
                        Wht_65Over_NoDis = "B18101A_010",
                        Blk_Under18_Dis = "B18101B_003",
                        Blk_Under18_NoDis = "B18101B_004",
                        Blk_18to64_Dis = "B18101B_006",
                        Blk_18to64_NoDis = "B18101B_007",
                        Blk_65Over_Dis = "B18101B_009",
                        Blk_65Over_NoDis = "B18101B_010",
                        NatAm_Under18_Dis = "B18101C_003",
                        NatAm_Under18_NoDis = "B18101C_004",
                        NatAm_18to64_Dis = "B18101C_006",
                        NatAm_18to64_NoDis = "B18101C_007",
                        NatAm_65Over_Dis = "B18101C_009",
                        NatAm_65Over_NoDis = "B18101C_010",
                        Asn_Under18_Dis = "B18101D_003",
                        Asn_Under18_NoDis = "B18101D_004",
                        Asn_18to64_Dis = "B18101D_006",
                        Asn_18to64_NoDis = "B18101D_007",
                        Asn_65Over_Dis = "B18101D_009",
                        Asn_65Over_NoDis = "B18101D_010",
                        NHPI_Under18_Dis = "B18101E_003",
                        NHPI_Under18_NoDis = "B18101E_004",
                        NHPI_18to64_Dis = "B18101E_006",
                        NHPI_18to64_NoDis = "B18101E_007",
                        NHPI_65Over_Dis = "B18101E_009",
                        NHPI_65Over_NoDis = "B18101E_010",
                        Othr_Under18_Dis = "B18101F_003",
                        Othr_Under18_NoDis = "B18101F_004",
                        Othr_18to64_Dis = "B18101F_006",
                        Othr_18to64_NoDis = "B18101F_007",
                        Othr_65Over_Dis = "B18101F_009",
                        Othr_65Over_NoDis = "B18101F_010",
                        TwoPlus_Under18_Dis = "B18101G_003",
                        TwoPlus_Under18_NoDis = "B18101G_004",
                        TwoPlus_18to64_Dis = "B18101G_006",
                        TwoPlus_18to64_NoDis = "B18101G_007",
                        TwoPlus_65Over_Dis = "B18101G_009",
                        TwoPlus_65Over_NoDis = "B18101G_010")

ACS_variables_ethnicity <- c(NHWht_Under18_Dis = "B18101H_003",
                        NHWht_Under18_NoDis = "B18101H_004",
                        NHWht_18to64_Dis = "B18101H_006",
                        NHWht_18to64_NoDis = "B18101H_007",
                        NHWht_65Over_Dis = "B18101H_009",
                        NHWht_65Over_NoDis = "B18101H_010",
                        Ltn_Under18_Dis = "B18101I_003",
                        Ltn_Under18_NoDis = "B18101I_004",
                        Ltn_18to64_Dis = "B18101I_006",
                        Ltn_18to64_NoDis = "B18101I_007",
                        Ltn_65Over_Dis = "B18101I_009",
                        Ltn_65Over_NoDis = "B18101I_010")

ACS_county_raw <- get_acs(geography = "county", variables = ACS_variables_race,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="wide",
                          survey = ACS_product )

ACS_county_eth_raw <- get_acs(geography = "county", variables = ACS_variables_ethnicity,
                          state = statenumber, county=baycounties,
                          year=ACS_year,
                          output="tidy",
                          survey = ACS_product )

#Combine categories to increase meaningful MoE
Disability_county <- ACS_county_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  Asn_Under18_Dis= Asn_Under18_DisE+NHPI_Under18_DisE,
  Asn_Under18_NoDis= Asn_Under18_NoDisE+NHPI_Under18_NoDisE,
  Asn_18to64_Dis= Asn_18to64_DisE+NHPI_18to64_DisE,
  Asn_18to64_NoDis=  Asn_18to64_NoDisE+NHPI_18to64_NoDisE,
  Asn_65Over_Dis= Asn_65Over_DisE+NHPI_65Over_DisE,
  Asn_65Over_NoDis= Asn_65Over_NoDisE+NHPI_65Over_NoDisE,
  Other_Under18_Dis= NatAm_Under18_DisE+Othr_Under18_DisE+TwoPlus_Under18_DisE,
  Other_Under18_NoDis= NatAm_Under18_NoDisE+Othr_Under18_NoDisE+TwoPlus_Under18_NoDisE,
  Other_18to64_Dis= NatAm_18to64_DisE+Othr_18to64_DisE+TwoPlus_18to64_DisE,
  Other_18to64_NoDis=  NatAm_18to64_NoDisE+Othr_18to64_NoDisE+TwoPlus_18to64_NoDisE,
  Other_65Over_Dis= NatAm_65Over_DisE+Othr_65Over_DisE+TwoPlus_65Over_DisE,
  Other_65Over_NoDis= NatAm_65Over_NoDisE+Othr_65Over_NoDisE+TwoPlus_65Over_NoDisE
)

Disability_eth_county <- ACS_county_eth_raw %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),  # Keep portion of string before comma
  Year=ACS_year,
  Ethnicity = sapply((strsplit(as.character(variable),'_')),function(x) x[1]),
  Age = sapply((strsplit(as.character(variable),'_')),function(x) x[2]),
  Disability_Status= sapply((strsplit(as.character(variable),'_')),function(x) x[3])
  ) %>% rename(Population = estimate) %>% mutate(
    Ethnicity = replace(Ethnicity, Ethnicity == 'NHWht', 'Non-Hispanic White'))%>% mutate (
    Ethnicity = replace(Ethnicity, Ethnicity == 'Ltn', 'Latino'))%>% mutate (
    Disability_Status = replace(Disability_Status, Disability_Status == 'Dis', 'With Disabilities')) %>% mutate(
    Disability_Status = replace(Disability_Status, Disability_Status == 'NoDis', 'Without Disabilities'))%>% 
  select(Geo,GEOID, Year,Ethnicity,Age,Disability_Status,Population)

#test aggregate MOEs
Disability_county_agg <- Disability_county %>%
  summarize(sumBlkUnder18 = sum(Blk_Under18_DisE), 
            sumBlkUnder18 = moe_sum(Blk_Under18_DisM, Blk_Under18_DisE))
#Select what to print out
disability_race <- Disability_county %>% 
  select(Geo, Year, Wht_Under18_DisE, Wht_Under18_NoDisE, Wht_18to64_DisE, Wht_18to64_NoDisE, Wht_65Over_DisE, Wht_65Over_NoDisE,
         Blk_Under18_DisE, Blk_Under18_NoDisE, Blk_18to64_DisE, Blk_18to64_NoDisE,Blk_65Over_DisE, Blk_65Over_NoDisE,
         Asn_Under18_Dis, Asn_Under18_NoDis, Asn_18to64_Dis, Asn_18to64_NoDis, Asn_65Over_Dis, Asn_65Over_NoDis,
         Other_Under18_Dis, Other_Under18_NoDis, Other_18to64_Dis, Other_18to64_NoDis, Other_65Over_Dis, Other_65Over_NoDis
         )

#Select what to print out
disability_ethnicity <- Disability_eth_county %>% 
  select(Geo, Year, NHWht_Under18_DisE, NHWht_Under18_NoDisE, NHWht_18to64_DisE, NHWht_18to64_NoDisE, NHWht_65Over_DisE, NHWht_65Over_NoDisE,
         Ltn_Under18_DisE, Ltn_Under18_NoDisE, Ltn_18to64_DisE, Ltn_18to64_NoDisE,Ltn_65Over_DisE, Ltn_65Over_NoDisE
        )
# Write out CSV 
setwd("C:/Users/jhalpern/Documents/R/PBA2050/Outputs/DemographicsCoC")
write.csv(disability_race, file = "Disabilities/disability_age_race_2018.csv")
write.csv(Disability_eth_county, file = "Disabilities/disability_age_ethnicity_2018.csv")
