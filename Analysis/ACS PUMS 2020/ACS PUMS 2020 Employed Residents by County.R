# ACS PUMS 2020 Employed Residents by County.R
# 2020 1-year PUMS data

# Import Library

suppressMessages(library(tidyverse))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2020/pbayarea20.Rdata"
load (PERSON_RDATA)
USERPROFILE          <- gsub("\\\\","/", Sys.getenv("USERPROFILE"))
petrale              <- file.path(USERPROFILE,"Documents","GitHub","petrale","applications")
output               <- file.path(petrale,"travel_model_lu_inputs","2020","Workers")

# Set ESR codes for employed persons
# People excluded are those under 16 and/or not in the labor force (codes "NA" and "6" for ESR)

ESR_codes <- c("1", #Civilian employed, at work
               "2", #Civilian employed, with a job but not at work 
               "4", #Armed forces, at work
               "5")  #Armed forces, with a job but not at work 

# ESR==6 is not in labor force and omitted from this universe

workers <- pbayarea20 %>% 
  select(PUMA,County_Name,PWGTP,ESR) %>%
  filter(ESR %in% ESR_codes) %>% 
  group_by(County_Name) %>% 
  summarize(total=sum(PWGTP))

print(sum(workers$total))

write.csv(workers, paste0(output, "ACS PUMS 2020 Employed Residents by County.csv"), row.names = FALSE, quote = T)


 
