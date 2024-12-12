USAGE = "
 Summarize Non-Insitutionalized Group Quarters persons for the given ACS5-year dataset.

 Reads M:/Data/Census/PUMS/PUMS YYYY-YY/[hp]bayarea[YY-YY].Rdata
 Outputs the following file into the same directory:
 NonInstitutionalGroupQuartersPersonSummary.csv
 Columns include:
  County_Name,
  gq_mil = persons in group quarters who are military
  gq_univ = persons in group quarters who are university students
  gq_oth = oter persons in group quarters
    Note: the three categories are mutually exclusive
  gq_tot = gq_mil + gq_univ + gq_oth
  workers = persons who are employed
  AGE_0004, AGE_0519, AGE_2044, AGE_4564, AGE_65P = persons in group quarters by the TM1 age categories

"

suppressMessages({
  library(tidyverse)
  library(argparser)
})
options(width = 200)

argparser <- arg_parser(USAGE, hide.opts=TRUE)
argparser <- add_argument(parser=argparser, arg="PUMS", help="Survey (one of '2017-21' or '2018-22')")
# parse the command line arguments
argv <- parse_args(argparser)
stopifnot(argv$PUMS %in% c("2017-21","2018-22"))

pums_short <- paste0(substr(argv$PUMS,3,4), substr(argv$PUMS,6,7))
output_prefix <- paste0("ACSPUMS",str_replace(argv$PUMS,"-","-20"))
print(paste("PUMS:", argv$PUMS))
print(paste("pums_short:",pums_short))
print(paste("output_prefix:",output_prefix))

HOUSEHOLD_RDATA = paste0("M:/Data/Census/PUMS/PUMS ",argv$PUMS,"/hbayarea",pums_short,".Rdata")
PERSON_RDATA = paste0("M:/Data/Census/PUMS/PUMS ",argv$PUMS,"/pbayarea",pums_short,".Rdata")
OUTPUT_DIR = paste0("M:/Data/Census/PUMS/PUMS ",argv$PUMS)

print(paste("Loading",HOUSEHOLD_RDATA))
load (HOUSEHOLD_RDATA)
print(paste("Loading",PERSON_RDATA))
load (PERSON_RDATA)

if (argv$PUMS == "2017-21") {
  pbayarea <- pbayarea1721
  remove(pbayarea1721)
  hbayarea <- hbayarea1721
  remove(hbayarea1721)
} else {
  # rename for backwards compat
  pbayarea <- pbayarea %>% rename(PUMA = PUMA20)
  hbayarea <- hbayarea %>% rename(PUMA = PUMA20)
}

noninst_gq <- left_join(pbayarea,hbayarea, 
    by=c("PUMA", "SERIALNO", "ST", "ADJINC", "COUNTY", "County_Name", "PUMA_Name")) %>%
  select(SERIALNO,PUMA,COUNTY,County_Name,PUMA_Name,PWGTP,WGTP,TYPEHUGQ,ESR,NP,AGEP,MIL,SCH) %>%
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


noninst_gq <- noninst_gq %>% mutate(
    worker_PWGTP = ifelse(ESR %in% c(1,2,4,5), PWGTP, 0),
    gq_type = case_when(
        MIL==1 ~ "gq_mil",
        SCH==2 ~ "gq_univ", # these take precedence over MIL==3
        SCH==3 ~ "gq_univ",
        MIL==3 ~ "gq_mil",
        .default = "gq_oth"
    ),
    age_cat = case_when(
        AGEP < 5 ~ "AGE_0004",
        (AGEP >=  5) & (AGEP <= 19) ~ "AGE_0519",
        (AGEP >= 20) & (AGEP <= 44) ~ "AGE_2044",
        (AGEP >= 45) & (AGEP <= 64) ~ "AGE_4564",
        (AGEP >= 65) ~ "AGE_65P",
        .default = "AGE_unknown"
    )
)
print("head(noninst_gq)")
print(head(noninst_gq))

# summarize by gq category
gq_type_summary <- noninst_gq %>%
    group_by(County_Name, gq_type) %>%
    summarize(PWGTP=sum(PWGTP)) %>% 
    pivot_wider(names_from=gq_type, values_from=PWGTP) %>%
    mutate(gq_tot = gq_mil + gq_univ + gq_oth)

# summarize workers
gq_worker_summary <- noninst_gq %>%
    group_by(County_Name) %>%
    summarize(workers=sum(worker_PWGTP))

# summarize age
gq_age_summary <- noninst_gq %>%
    group_by(County_Name, age_cat) %>%
    summarize(PWGTP=sum(PWGTP)) %>% 
    pivot_wider(names_from=age_cat, values_from=PWGTP, values_fill=0)

gq_summary <- full_join(gq_type_summary, gq_worker_summary)
gq_summary <- full_join(gq_summary, gq_age_summary)
print(gq_summary)

output_file <- file.path(OUTPUT_DIR,"NonInstitutionalGroupQuartersPersonSummary.csv")
write.csv(gq_summary,output_file,row.names = FALSE)
print(paste("Wrote",output_file))