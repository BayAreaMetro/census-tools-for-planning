# ACS 2017 PUMS Auto Availability by County Senior and Disabled.R
# Analyze PUMS data for auto availability by household presence of seniors and disabled
# 2017 1-year PUMS data

# Import Library

suppressMessages(library(dplyr))

# Input person PUMS file

PERSON_RDATA = "M:/Data/Census/PUMS/PUMS 2017/pbayarea17.Rdata"
HH_RDATA = "M:/Data/Census/PUMS/PUMS 2017/hbayarea17.Rdata"
OUTPUT = "M:/Data/Requests/Alex Ghenis (WID)/PUMS2017/"
load (PERSON_RDATA)
load (HH_RDATA)

# Keep variables of interest and join person and household files 

person <- pbayarea17 %>% 
  select(SERIALNO,PUMA,County_Name,PWGTP,AGEP,DIS,SPORDER) %>%
  mutate(
    Senior=if_else(AGEP>=65,"Yes","No"),
    Disabled=if_else(DIS==1,"Yes","No"),
    Either_Senior_Disabled=if_else((AGEP>=65 | DIS==1),"Yes","No")
  )

household <- hbayarea17 %>%
  filter(NP!=0,BLD>=1) %>%                      # Filter out vacant and GQ households
  select(SERIALNO,VEH,WGTP) 
  

merged <- left_join(household,person,by="SERIALNO") %>% mutate(
  zero_car=if_else(VEH==0,PWGTP,0L),
  one_or_more_cars=if_else(VEH>=1,PWGTP,0L),
  total=PWGTP
)


# Summarize people living in households by 0,1+ vehicles

sum.senior<- merged %>%
  group_by(County_Name,Senior) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)

sum.disabled <- merged %>%
  group_by(County_Name,Disabled) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)

sum.either <- merged %>%
  group_by(County_Name,Either_Senior_Disabled) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)
  
# Export all the files

write.csv(sum.senior, paste0(OUTPUT, "ACS PUMS 2017 Seniors Living in Households with Zero Vehicles.csv"), row.names = FALSE, quote = T)
write.csv(sum.disabled, paste0(OUTPUT, "ACS PUMS 2017 PWD Living in Households with Zero Vehicles.csv"), row.names = FALSE, quote = T)
write.csv(sum.either, paste0(OUTPUT, "ACS PUMS 2017 Either Seniors or PWD Living in Households with Zero Vehicles.csv"), row.names = FALSE, quote = T)

# Now for households

household1 <- pbayarea17 %>% 
  select(SERIALNO,AGEP,DIS) %>%
  mutate(
    Senior=if_else(AGEP>=65,1L,0L),
    Disabled=if_else(DIS==1,1L,0L),
    Either_Senior_Disabled=if_else((AGEP>=65 | DIS==1),1L,0L)
  ) %>%
  group_by(SERIALNO) %>%
  summarize(Senior=sum(Senior),Disabled=sum(Disabled),Either_Senior_Disabled=sum(Either_Senior_Disabled)) %>%   mutate(
    Senior=if_else(Senior>0,"Yes","No"),
    Disabled=if_else(Disabled>0,"Yes","No"),
    Either_Senior_Disabled=if_else(Either_Senior_Disabled>0,"Yes","No")
  )

weight <- hbayarea17 %>%
  filter(NP!=0,BLD>=1) %>%                      # Filter out vacant and GQ households
  select(SERIALNO,WGTP,VEH,County_Name) 


household2 <- left_join(weight,household1, by="SERIALNO") %>% mutate(
  zero_car = if_else(VEH==0,WGTP,0L),
  one_or_more_cars = if_else(VEH>0,WGTP,0L),
  total = WGTP)


# Summarize households by vehicles and presence of seniors, PWD, or either

sum.hhsenior<- household2 %>%
  group_by(County_Name,Senior) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)

sum.hhdisabled <- household2 %>%
  group_by(County_Name,Disabled) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)

sum.hheither <- household2 %>%
  group_by(County_Name,Either_Senior_Disabled) %>%
  summarize(Zero_Car=sum(zero_car),One_or_More_cars=sum(one_or_more_cars),Total=sum(total)) %>% mutate(
    Share_Zero_Car=Zero_Car/Total)

# Export all the household files

write.csv(sum.hhsenior, paste0(OUTPUT, "ACS PUMS 2017 Households with Seniors with Zero Vehicles.csv"), row.names = FALSE, quote = T)
write.csv(sum.hhdisabled, paste0(OUTPUT, "ACS PUMS 2017 Households with PWD with Zero Vehicles.csv"), row.names = FALSE, quote = T)
write.csv(sum.hheither, paste0(OUTPUT, "ACS PUMS 2017 Households with Either Seniors or PWD with Zero Vehicles.csv"), row.names = FALSE, quote = T)

 