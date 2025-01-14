setwd("C:/Users/dying/Documents/f_Data") # Set working directory
library(dplyr)
library(tidyr)
library(ggplot2)

# Load model outputs (Number = round, a = 2015, b = 2050)
parcels_geography = read.csv(file="07_11_2019_parcels_geography.csv",head=TRUE, sep=",") %>%
  unite("trich_hra", trich_id, hra_id) %>% #Combine Trich and HRA ID columns
  select(geom_id, trich_hra) #Limit to only relevant columns
parcels2010 = read.csv(file="parcels2010.csv",head=TRUE, sep=",") %>%
  select(PARCEL_ID, GEOM_ID, geom_id_s)
rtff_1a = read.csv(file="run26_parcel_data_2015.csv",head=TRUE, sep=",") %>%
  mutate(units_r1a = total_residential_units,
         hh_r1a = tothh) %>% #Assign unique names to columns
  select(parcel_id, units_r1a, hh_r1a) 
rtff_1b = read.csv(file="run26_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_r1b = total_residential_units,
         hh_r1b = tothh) %>%
  select(parcel_id, units_r1b, hh_r1b)
rtff_2 = read.csv(file="run15r_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_r2 = total_residential_units,
         hh_r2 = tothh) %>%
  select(parcel_id, units_r2, hh_r2)
cag_1a = read.csv(file="run28_parcel_data_2015.csv",head=TRUE, sep=",") %>%
  mutate(units_c1a = total_residential_units,
         hh_c1a = tothh) %>%
  select(parcel_id, units_c1a, hh_c1a)
cag_1b = read.csv(file="run28_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_c1b = total_residential_units,
         hh_c1b = tothh) %>%
  select(parcel_id, units_c1b, hh_c1b)
cag_2 = read.csv(file="run15c_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_c2 = total_residential_units,
         hh_c2 = tothh) %>%
  select(parcel_id, units_c2, hh_c2)
bttf_1a = read.csv(file="run27_parcel_data_2015.csv",head=TRUE, sep=",") %>%
  mutate(units_b1a = total_residential_units,
         hh_b1a = tothh) %>%
  select(parcel_id, units_b1a, hh_b1a)
bttf_1b = read.csv(file="run27_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_b1b = total_residential_units,
         hh_b1b = tothh) %>%
  select(parcel_id, units_b1b, hh_b1b)
bttf_2 = read.csv(file="run15b_parcel_data_2050.csv",head=TRUE, sep=",") %>%
  mutate(units_b2 = total_residential_units,
         hh_b2 = tothh) %>%
  select(parcel_id, units_b2, hh_b2)  

#Join data frames
parcels = inner_join(parcels_geography, parcels2010, by = c("geom_id" = "GEOM_ID"), copy = FALSE, suffix = c(".x", ".y")) #Join HRA assignments with parcel ID
rtff_join1 = inner_join(rtff_1a, rtff_1b, by = NULL, copy = FALSE, suffix = c(".x", ".y")) #Join future round 1 2015 and 2050 data
rtff_join2 = inner_join(rtff_join1, rtff_2, by = NULL, copy = FALSE, suffix = c(".x", ".y")) #Join future round 1 to future round 2
rtff_join = inner_join(parcels, rtff_join2, by = c("PARCEL_ID" = "parcel_id"), copy = FALSE, suffix = c(".x", ".y")) %>% #Join future rounds to HRA classification
  replace_na(list(hh_r1a = 0, hh_r1b = 0, hh_r2 =0)) #Replace NA with 0
cag_join1 = inner_join(cag_1a, cag_1b, by = NULL, copy = FALSE, suffix = c(".x", ".y"))
cag_join2 = inner_join(cag_join1, cag_2, by = NULL, copy = FALSE, suffix = c(".x", ".y"))
cag_join = inner_join(parcels, cag_join2, by = c("PARCEL_ID" = "parcel_id"), copy = FALSE, suffix = c(".x", ".y")) %>%
  replace_na(list(hh_c1a = 0, hh_c1b = 0, hh_c2 =0))
bttf_join1 = inner_join(bttf_1a, bttf_1b, by = NULL, copy = FALSE, suffix = c(".x", ".y"))
bttf_join2 = inner_join(bttf_join1, bttf_2, by = NULL, copy = FALSE, suffix = c(".x", ".y"))
bttf_join = inner_join(parcels, bttf_join2, by = c("PARCEL_ID" = "parcel_id"), copy = FALSE, suffix = c(".x", ".y")) %>%
  replace_na(list(hh_b1a = 0, hh_b1b = 0, hh_b2 =0))

#Calculate sums
rtff = rtff_join %>% group_by(trich_hra) %>% #Group by Trich and HRA category
  summarize(r1a = sum(hh_r1a), 
            r1b = sum(hh_r1b),
            r2 = sum(hh_r2)) #Sum by categories
cag = cag_join %>% group_by(trich_hra) %>%
  summarize(c1a = sum(hh_c1a),
            c1b = sum(hh_c1b),
            c2 = sum(hh_c2))
bttf = bttf_join %>% group_by(trich_hra) %>%
  summarize(b1a = sum(hh_b1a),
            b1b = sum(hh_b1b),
            b2 = sum(hh_b2))
#Export
write.csv(rtff, "rtff.csv")
write.csv(cag, "cag.csv")
write.csv(bttf, "bttf.csv")