# -------------------------------------------------------------------------
# This script downloads PUMS data to provide context for Transit Passenger Survey data analysis

# It adds labels to the data (as this step seems to be easer in R than in Tableau)
# It exports the data to csv, which can be read by Tableau for further visualization
# This is a person table with household income
# -------------------------------------------------------------------------

# Load libraries
library(tidycensus)
library(tidyverse)
library(kableExtra)

# My Census API key is saved in my .Renviron file so I can comment out the following
# census_api_key("your_api_key_here", install = TRUE)

# Get PUMAs for the Bay Area
# Borrowing the list from https://github.com/RSGInc/mtc_hts_weighting_scripts/blob/e4371fc485afd6d14f80bbe3acff10c28eaf0542/configs/scenarios/rMove_only.yaml#L74C1-L87C89
# But with Marin, Napa and Sonoma broken up into separate lists
puma_groups <- list(
  Alameda_W = c("00101", "00111", "00112", "00113", "00114", "00115", "00116", "00117", "00118", "00121", "00122", "00123"),
  Alameda_E = c("00119", "00120"),
  Contra_Costa = c("01301",  "01310", "01311", "01312", "01305", "01308", "01309", "01313", "01314"),
  San_Francisco_NE = c("07509", "07510", "07514"),
  San_Francisco_SE = c("07507", "07511"),
  San_Francisco_SW = c("07512", "07513"),
  San_Francisco_NW = c("07508"),
  San_Mateo = c("08101", "08102", "08103", "08104", "08105", "08106"),
  Santa_Clara = c("08506", "08507", "08512", "08505", "08508", "08511", "08510", "08519", "08522", "08521", "08517", "08516", "08515", "08520", "08518"),
  Solano = c("09501", "09503", "09502"),
  Marin  = c("04103", "04104"),
  Napa   = c("05500"),
  Sonoma = c("09705", "09706", "09702", "09704")
)

# Combine all PUMAs into one list
all_BayAreaPumas <- unlist(puma_groups)

# Download PUMS data
pums_df <- get_pums(
  state = "CA",
  puma = all_BayAreaPumas,
  year = 2023,
  survey = "acs1",
  variables = c(
         "HINCP",    # Household income
         "ADJINC",   # Adjustment factor for income and earnings dollar amounts (so it's representative of a standard month)
         "VEH",      # Vehicle owership
         "RAC1P"     # Race
  )
)



# --------------------------------------------------------------------------------
# Add labels
# --------------------------------------------------------------------------------
# check data type
typeof(pums_df$HINCP)
typeof(pums_df$ADJINC)
typeof(pums_df$VEH)
typeof(pums_df$RAC1P)

pums_df <- pums_df %>%
  
 # Convert HHT and VEH to character to handle both numeric and non-numeric values (e.g., "b") -- this step is no longer needed in PUMS2023
 # mutate(
 #   VEH = as.character(VEH)
 # ) %>%
  
  # Convert HINCP and ADJINC to numeric; non-numeric values will become NA
  mutate(
    HINCP_numeric = as.numeric(HINCP),
    ADJINC_numeric = as.numeric(ADJINC)
  ) %>%
 
  
  # Calculate adjusted income
  mutate(income= HINCP_numeric*ADJINC_numeric)    %>% # Need to confirm that the adjustment is needed

  # Add labels
  mutate( 
    Vehicle_Ownership_Label = case_when(
      VEH == "0" ~ "0 vehicle available",
      VEH == "1" ~ "1 vehicle available",
      VEH == "2" ~ "2 vehicles available",
      VEH == "3" ~ "3 vehicles available",
      VEH == "4" ~ "4 vehicles available",
      VEH == "5" ~ "5 vehicles available",
      VEH == "6" ~ "6 or more vehicles available",
      VEH == "-1" ~ "N/A (GQ/vacant)",  
      TRUE ~ "Unknown"
    ),
    Income_Label = case_when(
    income < 50000                       ~ "1. Under $50,000",
    income >= 50000 & income  < 100000   ~ "2. $50,000-$99,999",
    income >= 100000 & income < 150000   ~ "3. $100,000-$150,999",
    income >= 150000                     ~ "4. $150,000 or more"
    ),
  )

# Define a function to label PUMAs
label_pumas <- function(puma_code) {
  if (puma_code %in% puma_groups[["Alameda_W"]] || puma_code %in% puma_groups[["Alameda_E"]]) {
    return("Alameda")
  } else if (puma_code %in% puma_groups[["Contra_Costa"]]) {
    return("Contra Costa") 
  } else if (puma_code %in% puma_groups[["San_Francisco_NE"]] || 
             puma_code %in% puma_groups[["San_Francisco_SE"]] || 
             puma_code %in% puma_groups[["San_Francisco_SW"]] || 
             puma_code %in% puma_groups[["San_Francisco_NW"]]) {
    return("San Francisco")
  } else if (puma_code %in% puma_groups[["San_Mateo"]]) {
    return("San Mateo") 
  } else if (puma_code %in% puma_groups[["San_Clara"]]) {
    return("San Clara") 
  } else if (puma_code %in% puma_groups[["Solano"]]) {
    return("Solano") 
  } else if (puma_code %in% puma_groups[["Marin"]]) {
    return("Marin") 
  } else if (puma_code %in% puma_groups[["Napa"]]) {
    return("Napa") 
  } else if (puma_code %in% puma_groups[["Sonoma"]]) {
    return("Sonoma") 
  } else {
    return("Other")
  }
}

# Add the labels to the PUMS data
pums_df <- pums_df %>%
  mutate(County = sapply(PUMA, label_pumas))


# done labeling


# --------------------------------------------------------------------------------
# export the data to csv for easy visualization in Tableau
# --------------------------------------------------------------------------------
write_csv(pums_df, "E:/PUMS2023_SelectedVars_person.csv")


# --------------------------------------------------------------------------------
# Tabulations
# --------------------------------------------------------------------------------

# get income distribution by county (for quick verification of total persons e.g. against B01003 on data.census.gov)
Table_IncDistrByCounty <- pums_df %>%
  group_by(County, Income_Label) %>%  
  summarise(weight_sum = sum(PWGTP, na.rm = TRUE))

print(Table_IncDistrByCounty, n = Inf)

# get income distribution for the region
Table_IncDistr_Region <- pums_df %>%
  group_by(Income_Label) %>%  
  summarise(weight_sum = sum(PWGTP, na.rm = TRUE))
print(Table_IncDistr_Region, n = Inf)

# tabulate vehicle ownership
Table_VehOwn <- pums_df %>%
  group_by(VEH) %>%  
  summarise(weight_sum = sum(PWGTP, na.rm = TRUE))

print(Table_VehOwn, n = Inf)


