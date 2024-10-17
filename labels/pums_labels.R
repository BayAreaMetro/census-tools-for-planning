# PUMAs for the Bay Area
puma_lists <- list(
    alameda       <- c("00101","00111","00112","00113","00114","00115","00116","00117","00118","00119","00120","00121","00122","00123"),
    contra_costa  <- c("01301","01305","01308","01309","01310","01311","01312","01313","01314"),
    marin         <- c("04103","04104"),
    napa          <- c("05500"),
    san_francisco <- c("07507","07508","07509","07510","07511","07512","07513","07514"),
    san_mateo     <- c("08101","08102","08103","08104","08105","08106"),
    santa_clara   <- c("08505","08506","08507","08508","08510","08511","08512","08515","08516","08517","08518","08519","08520","08521","08522"),
    solano        <- c("09501", "09502", "09503"),
    sonoma        <- c("09702","09704","09705","09706")
)

# Combine all PUMAs into one list
all_BayAreaPumas_list <- unlist(puma_lists)

# Function to label home county based on PUMA
label_home_county <- function(PUMA) {
  case_when(
    PUMA %in% alameda       ~ "Alameda",
    PUMA %in% contra_costa  ~ "Contra Costa",
    PUMA %in% marin         ~ "Marin",
    PUMA %in% napa          ~ "Napa",
    PUMA %in% san_francisco ~ "San Francisco",
    PUMA %in% san_mateo     ~ "San Mateo",
    PUMA %in% santa_clara   ~ "Santa Clara",
    PUMA %in% solano        ~ "Solano",
    PUMA %in% sonoma        ~ "Sonoma",
    TRUE                    ~ "Should not be included in this extraction"
  )
}


# Function to label work county based on POWPUMA
label_work_county <- function(POWPUMA, POWSP) {
  case_when(
    POWPUMA == "00100"  & POWSP == "006" ~ "Alameda",
    POWPUMA == "01300"  & POWSP == "006" ~ "Contra Costa",
    POWPUMA == "04100"  & POWSP == "006" ~ "Marin",
    POWPUMA == "05500"  & POWSP == "006" ~ "Napa",
    POWPUMA == "07500"  & POWSP == "006" ~ "San Francisco",
    POWPUMA == "08100"  & POWSP == "006" ~ "San Mateo",
    POWPUMA == "08500"  & POWSP == "006" ~ "Santa Clara",
    POWPUMA == "09500"  & POWSP == "006" ~ "Solano",
    POWPUMA == "09700"  & POWSP == "006" ~ "Sonoma",
    POWPUMA == "N"                     ~ "zNot Coded",
    TRUE                               ~ "zOut of Region"
  )
}


# Function to label service worker status based on OCCP
label_service_worker <- function(OCCP) {
  case_when(
    OCCP == "000N"                 ~ "Not_Coded", # for PUMS 2022
    OCCP == "N"                    ~ "Not_Coded", # for PUMS 2023
    OCCP >= 3601 & OCCP <= 4655    ~ "Service_worker",
    TRUE                           ~ "Not_service_worker"
  )
}


# Function to label travel to work mode based on JWTRNS
label_journey_to_work_mode <- function(JWTRNS, JWRIP) {
  case_when(
    JWTRNS==0                                     ~ "0. Not Coded",
    JWTRNS==1                                     ~ "1. Drive",
    JWTRNS %in% c("2","3","4","5","6")            ~ "2. Transit",
    JWTRNS %in% c("7","8")                        ~ "8. Other",
    JWTRNS=="9"                                   ~ "3. Bicycle",
    JWTRNS=="10"                                  ~ "4. Walk",
    JWTRNS=="11"                                  ~ "5. Worked from home",
    JWTRNS=="12"                                  ~ "8. Other",
    TRUE                                          ~ "Miscoded"
  )
}
