# ACS PUMS 2019 and 2020 Experimental Weights Comparison.r
# Create a single file with 2019 standard, 2019 experimental, and 2020 experimental weights
# Do the analysis in Tableau

# Import Libraries

suppressMessages(library(tidyverse))

# Set up working directory

USERPROFILE <- gsub("////","/", Sys.getenv("USERPROFILE"))
BOX_TM      <- file.path(USERPROFILE, "Box", "Modeling and Surveys", "Census","2020","PUMS Experimental Weights")

# Data locations

pums2019                 <- "M:/Data/Census/PUMS/PUMS 2019"
pums2020                 <- "M:/Data/Census/PUMS/PUMS 2020"
pums2019_experimental_in <- "M:/Data/Census/PUMS/PUMS 2019/csv_hpus_expwgt/National_NoRW.csv"

# Bring in data - experimental weights, 2019 HH and person files, 2020 HH and person files

pums2019_experimental           <- read.csv(pums2019_experimental_in)

load(file.path(pums2019,"hbayarea19.Rdata"))
load(file.path(pums2019,"pbayarea19.Rdata"))

load(file.path(pums2020,"hbayarea20.Rdata"))
load(file.path(pums2020,"pbayarea20.Rdata"))

# Join 2019 experimental weights with person file and then household file 
# Note: the weight of the person with RELSHIPP = 20 (on the main PUMS file) is the experimental housing unit weight
# To facilitate apples to apples comparison, removing vacant units and group quarters
# Add year variables and rename weights for 2019, experimental 2019, and 2020 (experimental)

pbayarea19_exp <- left_join(pbayarea19,pums2019_experimental,by=c("SERIALNO"="serialno","SPORDER"="sporder")) %>% 
  relocate(PWGTP,.before = ebw_pu_final) %>% 
  filter(RELSHIPP<37) %>% 
  rename(PWGTP_2019_exp=ebw_pu_final,PWGTP_2019=PWGTP) %>% 
  mutate(YEAR=2019)

relshipp <- pbayarea19_exp %>% 
  filter(RELSHIPP==20) %>% 
  select(SERIALNO,PWGTP_2019_exp)

hbayarea19_exp <- left_join(hbayarea19,relshipp,by="SERIALNO") %>% 
  filter(NP>=1,TYPE==1) %>% 
  rename(WGTP_2019_exp=PWGTP_2019_exp,WGTP_2019=WGTP) %>% 
  mutate(YEAR=2019)

hbayarea20_exp <- hbayarea20 %>% 
  filter(NP>=1,TYPEHUGQ==1) %>% 
  mutate(YEAR=2020) %>% 
  rename(WGTP_2020=WGTP)

pbayarea20_exp <- pbayarea20 %>% 
  filter(RELSHIPP<37) %>% 
  mutate(YEAR=2020) %>% 
  rename(PWGTP_2020=PWGTP)

# Concatenate person and then household files for 2019 and 2020 into single files, respectively

pbayarea19_20_exp <- bind_rows(pbayarea19_exp,pbayarea20_exp) %>% 
  mutate_at(vars(PWGTP_2019,PWGTP_2019_exp,PWGTP_2020),~ifelse(is.na(.),0,.))

hbayarea19_20_exp <- bind_rows(hbayarea19_exp,hbayarea20_exp) %>% 
  mutate_at(vars(WGTP_2019,WGTP_2019_exp,WGTP_2020),~ifelse(is.na(.),0,.))

# Export Files, first individual files, then concatenated ones

write.csv(pbayarea19_exp,file.path(BOX_TM,"pbayarea19_exp.csv"), row.names = F)
write.csv(hbayarea19_exp,file.path(BOX_TM,"hbayarea19_exp.csv"), row.names = F)
write.csv(pbayarea20_exp,file.path(BOX_TM,"pbayarea20_exp.csv"), row.names = F)
write.csv(hbayarea20_exp,file.path(BOX_TM,"hbayarea20_exp.csv"), row.names = F)

write.csv(pbayarea19_20_exp,file.path(BOX_TM,"pbayarea19_20_exp.csv"), row.names = F)
write.csv(hbayarea19_20_exp,file.path(BOX_TM,"hbayarea19_20_exp.csv"), row.names = F)
