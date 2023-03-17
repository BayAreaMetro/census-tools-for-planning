# ACS 2017-2021 PUMS Industry and Occupation.R
# Analyze PUMS data for industry and occupation 
# 2021 5-year PUMS data

# Import Library

library(tidyverse)

# Input person PUMS file and set output

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017-21/pbayarea1721.Rdata"
USERPROFILE  <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
OUTPUT = file.path(USERPROFILE, "Box/Modeling and Surveys/Surveys/Travel Diary Survey/Biennial Travel Diary Survey/2023 Survey Instrument Development/Industry and Occupation Summaries")
load (PERSON_RDATA)


# Set ESR codes of interest - basically anyone employed or unemployed, but in the labor force
# People excluded are those under 16 and/or not in the labor force (codes "NA" and "6" for ESR)
# ESR==3 is unemployed and ESR==6 is not in labor force and both omitted from this universe

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

# Recode industry and occupation as well as county of work (COW)
# Use first two digits of industry and occupation (3M is grouped in 33 and 4M is grouped in 45)

person <- pbayarea1721 %>% 
  select(PUMA,County_Name,POWPUMA, POWSP, PWGTP,NAICSP,SOCP,ESR) %>%
  filter(ESR %in% ESR_codes) %>% 
    mutate(
      NAICS2=substr(NAICSP,1,2),
      SOC2=substr(SOCP,1,2),
      NAICS_description=case_when(
        NAICS2==11	     ~         "Agriculture, Forestry, Fishing and Hunting", 
        NAICS2==21	     ~         "Mining, Quarrying, and Oil and Gas Extraction",
        NAICS2==22	     ~         "Utilities",
        NAICS2==23	     ~         "Construction",
        NAICS2>=31 & NAICS2<=33	~  "Manufacturing",
        NAICS2=="3M"	~            "Manufacturing",
        NAICS2==42	     ~         "Wholesale Trade",
        NAICS2>=44 & NAICS2<=45	~  "Retail Trade",
        NAICS2=="4M"	~            "Retail Trade",
        NAICS2>=48 & NAICS2<=49	~  "Transportation and Warehousing",
        NAICS2==51	     ~         "Information",
        NAICS2==52	     ~         "Finance and Insurance",
        NAICS2==53	     ~         "Real Estate and Rental and Leasing",
        NAICS2==54	     ~         "Professional, Scientific, and Technical Services",
        NAICS2==55	     ~         "Management of Companies and Enterprises",
        NAICS2==56	     ~         "Administrative and Support and Waste Management and Remediation Services",
        NAICS2==61	     ~         "Educational Services",
        NAICS2==62	     ~         "Health Care and Social Assistance",
        NAICS2==71	     ~         "Arts, Entertainment, and Recreation",
        NAICS2==72	     ~         "Accommodation and Food Services",
        NAICS2==81	     ~         "Other Services (except Public Administration)",
        NAICS2==92	     ~         "Public Administration"),
      SOC_description=case_when(
        SOC2==11         ~          "Management Occupations",
        SOC2==13         ~          "Business and Financial Operations Occupations",
        SOC2==15         ~          "Computer and Mathematical Occupations",
        SOC2==17         ~          "Architecture and Engineering Occupations",
        SOC2==19         ~          "Life, Physical, and Social Science Occupations",
        SOC2==21         ~          "Community and Social Service Occupations",
        SOC2==23         ~          "Legal Occupations",
        SOC2==25         ~          "Educational Instruction and Library Occupations",
        SOC2==27         ~          "Arts, Design, Entertainment, Sports, and Media Occupations",
        SOC2==29         ~          "Healthcare Practitioners and Technical Occupations",
        SOC2==31         ~          "Healthcare Support Occupations",
        SOC2==33         ~          "Protective Service Occupations",
        SOC2==35         ~          "Food Preparation and Serving Related Occupations",
        SOC2==37         ~          "Building and Grounds Cleaning and Maintenance Occupations",
        SOC2==39         ~          "Personal Care and Service Occupations",
        SOC2==41         ~          "Sales and Related Occupations",
        SOC2==43         ~          "Office and Administrative Support Occupations",
        SOC2==45         ~          "Farming, Fishing, and Forestry Occupations",
        SOC2==47         ~          "Construction and Extraction Occupations",
        SOC2==49         ~          "Installation, Maintenance, and Repair Occupations",
        SOC2==51         ~          "Production Occupations",
        SOC2==53         ~          "Transportation and Material Moving Occupations",
        SOC2==55         ~          "Military Occupations",
        ),
      COW=case_when(
        POWSP==6 & POWPUMA==100   ~ "Alameda",
        POWSP==6 & POWPUMA==1300  ~ "Contra Costa",
        POWSP==6 & POWPUMA==4100  ~ "Marin",
        POWSP==6 & POWPUMA==5500  ~ "Napa",
        POWSP==6 & POWPUMA==7500  ~ "San Francisco",
        POWSP==6 & POWPUMA==8100  ~ "San Mateo",
        POWSP==6 & POWPUMA==8500  ~ "Santa Clara",
        POWSP==6 & POWPUMA==9500  ~ "Solano",
        POWSP==6 & POWPUMA==9700  ~ "Sonoma",
        TRUE                      ~ "Not at work or out of region")
      )

write.csv(person, file.path(OUTPUT, "ACS PUMS 2021 5-year by industry and occupation.csv"), row.names = FALSE, quote = T)


 
