
# set working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
## load libraries
library("tidyverse")
library("haven")
library("car", pos = length(search()))
library("matrixStats")
# utility functions
source(file.path(PATH, "00-utils.r"))

## ccahs data
ccahs <- read_dta(file.path(PATH, "data", "CCAHS-2001-2003", "ICPSR-31142", "DS0001", "31142-0001-Data-REST.dta"))
ccahs <- ccahs %>%
    transmute(
        id          = V1,
        date        = as.Date(V8, format = "%d-%b-%Y"),
        weight      = V1901,
        # geo identifiers
        state       = "17",
        county      = "031",
        tract       = V1913,
        block_group = str_sub(V1915, -1),
        block       = V1916 %>% as.character(),
        nc          = V5,
        CTCode      = str_c("17031", V1913),
        BGCode      = V1915,
        CBCode      = str_c(CTCode, V1916),
        # std. demographic variables
        # race: 1=white, black=2, asian=3, hispanic=4, other=5
        race           = car::recode(V2003, "1=1;2=2;4=3;6=4;c(3,5)=5"),
        female         = car::recode(V104, "1=0;2=1") %>% as.numeric,
        income         = car::recode(V3503, "c(-3,-2)=NA") %>% as.numeric,
        edu_years      = V2001,
        age            = V2000,
        home_ownership = car::recode(V2068, "1=1;c(2,3)=0") %>% as.numeric,
        years_in_nhood = car::recode(ccahs$V1416, "1=1;c(-2,-3)=NA") %>% as.numeric,
        years_in_nhood = as.numeric(format(date, "%Y")) - years_in_nhood,
        # family_status: 1=married, 2=separated, 3=divorced, 4=widowed, 5=never married
        family_status = car::recode(V710, "-3=NA"),
        # emp status: 1=worked, 2=unemployed, 3=retired, 4=keeping home, 5=student, 6=disabled
        emp_status     = car::recode(V2704, "c(1,2,3,4,5)=1;6=2;7=3;8=4;9=5;10=6"),
        unemployed_5yr = car::recode(V1127, "1=1;5=0;c(-3,-2)=NA")  %>% as.numeric,
        foreign_born   = car::recode(V2013, "1:6=0;7=1"),
        immigrant_generation = car::recode(V2026, "4=0"),
        # V1127 .Last5yrs: Unemployed, looking for work for longer than 3 months
        # perceived neighborhood violence
        violence_perceived_scale   = V2808,
        # personal victimization
        crime_victim               = na_codes(V558, -3) == 1,
        # code missing values (no crappy ccahs imputation)
        home_ownership            = ifelse(V2091 == 1, NA, home_ownership),
        edu_years                 = ifelse(V2081 == 1, NA, edu_years),
        emp_status                = ifelse(V2782 == 1, NA, emp_status),
        race                      = ifelse(V2083 == 1, NA, race),
        immigrant_generation      = ifelse(V2089 == 1, NA, immigrant_generation),
        foreign_born              = ifelse(V2084 == 1, NA, foreign_born)
    )

## Join with census data
cook_cb <- read_rds(file.path(PATH, "data", "census-cook-cb-2000.rds")) 
ccahs_census <- ccahs %>% left_join(cook_cb@data, by = c("state", "county", "tract", "block"))

## write recoded dataset
write_rds(ccahs_census, file.path(PATH, "data", "ccahs-2001-2003-recoded.rds"), compress = "gz")

## quit
q(save = "no")
