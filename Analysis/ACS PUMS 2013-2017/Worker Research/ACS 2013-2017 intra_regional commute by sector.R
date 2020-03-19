# ACS 2013-2017 intra_regional commute by sector.R
# Analyze PUMS data for Bay Area commuters by industry 2017 5-year PUMS data
# Import Libraries

suppressMessages(library(tidyverse))

# Set working directory

wd <- "C:/Users/sisrael/Documents/GitHub/petrale/applications/travel_model_lu_inputs/2015/Employment/"
setwd(wd)

# Input person census files for workers working in the Bay, read in industry equivalency

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2013-17/pbayarea1317.Rdata"
eq           = "M:/Crosswalks/Census/Industry/NAICS to MTC Sectors.csv"
baypowpuma = c(100,1300,4100,5500,7500,8100,8500,9500,9700)

load (PERSON_RDATA)

equivalency <- read.csv(eq,header=TRUE) %>%    # Convert to character for later joining
  mutate(NAICS.2=as.character(NAICS.2))

workers <- pbayarea1317 %>% 
  filter(POWPUMA %in% baypowpuma) %>% 
  mutate(NAICS.2=substr(as.character(NAICSP),1,2)) %>%  # Convert NAICSP factor to character, extract left 2 characters
  mutate(NAICS.2=case_when(
    NAICS.2=="3M"   ~"33",                              # Recode industry codes with characters to numeric
    NAICS.2=="4M"   ~"45",
    TRUE            ~NAICS.2
  ))

combined <- left_join(workers,equivalency,by="NAICS.2") %>% 
  mutate(work_county=case_when(
    POWPUMA==100    ~"Alameda",
    POWPUMA==1300   ~"Contra Costa",
    POWPUMA==4100   ~"Marin",
    POWPUMA==5500   ~"Napa",
    POWPUMA==7500   ~"San Francisco",
    POWPUMA==8100   ~"San Mateo",
    POWPUMA==8500   ~"Santa Clara",
    POWPUMA==9500   ~"Solano",
    POWPUMA==9700   ~"Sonoma"
  )) 

# Summarize total workers by industry, reformat using spread function, and export

worker_summary <- combined %>%
  group_by(work_county,MTCname) %>%
  summarize(worker_total=sum(PWGTP)) %>% 
  spread(.,MTCname,worker_total,fill=0)

write.csv(worker_summary, "ACS PUMS 2013-2017 regional commuters by industry.csv", row.names = FALSE, quote = T)

