USAGE = "
 Summarize Non-Insitutionalized Group Quarters persons for the given ACS dataset.

 Reads M:/Data/Census/PUMS/PUMS YYYY[-YY]/[hp]bayarea[YY[-YY]].Rdata
 Outputs summaries/noninst_gq_summary.csv with columns:
  County_Name,
  gq_type_mil = persons in group quarters who are military
  gq_type_univ = persons in group quarters who are university students
  gq_type_othnon = oter persons in group quarters
    Note: the three categories are mutually exclusive
  gqpop = gq_type_mil + gq_type_univ + gq_type_othnon
  AGE0004, AGE0519, AGE2044, AGE4564, AGE65P = persons in group quarters by the TM1 age categories
  pers_occ_[management,professional,services,retail,manual,military] = occupation from SOCP
  EMPRES = persons who are employed
"

suppressMessages({
  library(tidyverse)
  library(argparser)
})
options(width = 200)

# See https://github.com/BayAreaMetro/populationsim/blob/ed898c7dec15c2c696440dc39bc0bd20c128458d/bay_area/create_seed_population.py#L109-L120
#    0: N/A
#    1: management
#    2: professional
#    3: services
#    4: retail
#    5: manual
#    6: military
SOC_to_occ <- data.frame( 
  "SOC"       =c("11","13","15","17","19","21","23","25","27","29","31","33","35","37","39","41","43","45","47","49","51","53","55"),
  "occupation"=c(    1,   2,   2,   2,   2,   3,   2,   2,   3,   2,   3,   3,   4,   5,   3,   4,   3,   5,   5,   5,   5,   5,   6)
)

argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="survey",  help="Either acs1 or acs5")
argparser <- add_argument(parser=argparser, arg="year",    help="Survey year", type="numeric")
# parse the command line arguments
argv <- parse_args(argparser)
stopifnot(argv$survey %in% c("acs1","acs5"))
stopifnot(argv$year >= 2020)

PUMS_ROOT_DIR <- "M:/Data/Census/PUMS"
if (argv$survey == "acs5") {
    PUMS_DIR <- file.path(PUMS_ROOT_DIR, sprintf("PUMS %d-%02d", argv$year-4, argv$year %% 100))
    PUMS_YEAR_STR <- sprintf("%02d%02d", (argv$year-4) %% 100, argv$year %% 100)
} else {
    PUMS_DIR <- file.path(PUMS_ROOT_DIR, sprintf("PUMS %d", argv$year))
    PUMS_YEAR_STR <- sprintf("%02d", argv$year %% 100)
}
print(paste("PUMS_DIR:", PUMS_DIR))
print(paste("PUMS_YEAR_STR:", PUMS_YEAR_STR))

HOUSEHOLD_RDATA = file.path(PUMS_DIR, paste0("hbayarea",PUMS_YEAR_STR,".Rdata"))
PERSON_RDATA    = file.path(PUMS_DIR, paste0("pbayarea",PUMS_YEAR_STR,".Rdata"))

print(paste("Loading",HOUSEHOLD_RDATA))
load (HOUSEHOLD_RDATA)
print(paste("Loading",PERSON_RDATA))
load (PERSON_RDATA)

if ((argv$survey == "acs5") & (argv$year == 2021)) {
  pbayarea <- pbayarea1721
  remove(pbayarea1721)
  hbayarea <- hbayarea1721
  remove(hbayarea1721)
} 

if ("PUMA20" %in% colnames(pbayarea)) {
  # rename for backwards compat
  pbayarea <- pbayarea %>% rename(PUMA = PUMA20)
  hbayarea <- hbayarea %>% rename(PUMA = PUMA20)
}
if ("STATE" %in% colnames(pbayarea)) {
  # rename for backwards compat
  pbayarea <- pbayarea %>% rename(ST = STATE)
  hbayarea <- hbayarea %>% rename(ST = STATE)
}

noninst_gq <- left_join(pbayarea,hbayarea, 
    by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPEHUGQ,ESR,NP,AGEP,MIL,SCH,SOCP) %>%
  filter(TYPEHUGQ==3)

print(paste("Filtered to", nrow(noninst_gq),"rows for noninstitional group quarters"))
print(paste("sum(noninst_gq$NP) = ",sum(noninst_gq$NP)))
print(paste("sum(noninst_gq$PWGTP) = ",sum(noninst_gq$PWGTP)))

# MIL Military service
#   b .N/A (less than 17 years old)
#   1 .Now on active duty
#   2 .On active duty in the past, but not now
#   3 .Only on active duty for training in Reserves/National Guard
#   4 .Never served in the military

# SCH School enrollment
#   b .N/A (less than 3 years old)
#   1 .No, has not attended in the last 3 months
#   2 .Yes, public school or public college
#   3 .Yes, private school, private college, or home school
print("table(noninst_gq$MIL, noninst_gq$SCH)")
print(table(noninst_gq$MIL, noninst_gq$SCH))


noninst_gq <- noninst_gq %>% 
  mutate(
    SOC = str_sub(SOCP, start = 1, end = 2),
    worker_PWGTP = ifelse(ESR %in% c(1,2,4,5), PWGTP, 0),
    gq_type = case_when(
        MIL==1 ~ "gq_type_mil",
        SCH==2 ~ "gq_type_univ", # these take precedence over MIL==3
        SCH==3 ~ "gq_type_univ",
        MIL==3 ~ "gq_type_mil",
        .default = "gq_type_othnon"
    ),
    age_cat = case_when(
        AGEP < 5 ~ "AGE0004",
        (AGEP >=  5) & (AGEP <= 19) ~ "AGE0519",
        (AGEP >= 20) & (AGEP <= 44) ~ "AGE2044",
        (AGEP >= 45) & (AGEP <= 64) ~ "AGE4564",
        (AGEP >= 65) ~ "AGE65P",
        .default = "AGEunknown"
    )
) %>% left_join(., SOC_to_occ, by="SOC") %>%
  mutate(
    occupation = case_when(
      occupation == 1 ~ "pers_occ_management",
      occupation == 2 ~ "pers_occ_professional",
      occupation == 3 ~ "pers_occ_services",
      occupation == 4 ~ "pers_occ_retail",
      occupation == 5 ~ "pers_occ_manual",
      occupation == 6 ~ "pers_occ_military",
      .default = "pers_occ_unknown"
    ))
print("head(noninst_gq)")
print(head(noninst_gq))

# summarize by gq category
gq_type_summary <- noninst_gq %>%
    group_by(County_Name, gq_type) %>%
    summarize(PWGTP=sum(PWGTP)) %>% 
    pivot_wider(names_from=gq_type, values_from=PWGTP, values_fill=0) %>%
    mutate(gqpop = gq_type_mil + gq_type_univ + gq_type_othnon)

# summarize by occupation
gq_worker_summary <- noninst_gq %>%
    group_by(County_Name, occupation) %>%
    summarize(worker_PWGTP=sum(worker_PWGTP)) %>%
    pivot_wider(names_from=occupation, values_from=worker_PWGTP, values_fill=0) %>%
    mutate(EMPRES = pers_occ_management + pers_occ_professional + pers_occ_services + 
                     pers_occ_retail + pers_occ_manual + pers_occ_military) %>%
    select(-pers_occ_unknown)

# summarize age
gq_age_summary <- noninst_gq %>%
    group_by(County_Name, age_cat) %>%
    summarize(PWGTP=sum(PWGTP)) %>% 
    pivot_wider(names_from=age_cat, values_from=PWGTP, values_fill=0)

gq_summary <- full_join(gq_type_summary, gq_worker_summary)
gq_summary <- full_join(gq_summary, gq_age_summary)

if (!("AGE0004" %in% colnames(gq_summary))) {
    gq_summary <- gq_summary %>% mutate(AGE0004=0)
}
print(gq_summary)

dir.create(file.path(PUMS_DIR, "summaries"), showWarnings = FALSE)
output_file <- file.path(PUMS_DIR,"summaries","noninst_gq_summary.csv")
write.csv(gq_summary,output_file,row.names = FALSE)
print(paste("Wrote",output_file))