
# working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
## load libraries
library("MASS")
library("tidyverse")
library("readr")
library("stargazer")
library("spdep")
library("purrr")
library("spdplyr")
library("rgeos")
library("maptools")
library("car", pos = length(search()))
# utility functions
proj4  <- CRS(" +proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
source(file.path(PATH, "00-utils.r"))

## Propagation of uncertainty from acs estimates (census blocks)
cook_bg <- read_rds(file.path(PATH, "data", "census-cook-bg-2010.rds"))
# standard errors from moe
x <- c("p_occ_management", "p_emp_unemployed", "p_poverty", "p_edu_hs", "p_hu_occupied_renter", "p_housing_moved_5yrs", "p_hu_vacant", "p_lang_eng_limited", "p_lang_eng_no", "hhinc_median", "hhinc_average") %>% paste0("_moe")
cook_bg <- cook_bg@data %>%
    mutate_at(vars(!!quo(x)), funs('se' = ./1.645)) %>%
    rename_at(vars(ends_with("moe_se")), funs(str_replace(., "_moe_se", "_se")))
# plausible values
cook_bg_pv <- 1:10 %>% map(~return(cook_bg)) %>% map(function(d) {
    d <- d %>%
        as_tibble() %>%
        # plausible values for acs variables
        mutate(
            p_occ_management       = rnorm(n(), p_occ_management, p_occ_management_se),
            p_emp_unemployed       = rnorm(n(), p_emp_unemployed, p_emp_unemployed_se),
            p_poverty              = rnorm(n(), p_poverty, p_poverty_se),
            p_edu_hs               = rnorm(n(), p_edu_hs, p_edu_hs_se),
            p_hu_occupied_renter   = rnorm(n(), p_hu_occupied_renter, p_hu_occupied_renter_se),
            p_housing_moved_5yrs   = rnorm(n(), p_housing_moved_5yrs, p_housing_moved_5yrs_se),
            p_hu_vacant            = rnorm(n(), p_hu_vacant, p_hu_vacant_se),
            p_lang_eng_limited     = rnorm(n(), p_lang_eng_limited, p_lang_eng_limited_se),
            p_lang_eng_no          = rnorm(n(), p_lang_eng_no, p_lang_eng_no_se),
            hhinc_median           = rnorm(n(), hhinc_median, hhinc_median_se),
            hhinc_average          = rnorm(n(), hhinc_average, hhinc_average_se),
            # Concentrated disad., resid. instab., immi. concentration
            con_disadv             = fac(cbind(p_occ_management, p_emp_unemployed, p_poverty, p_edu_hs, p_single_mother), factors = 1),
            res_instab             = fac(cbind(p_hu_occupied_renter, p_housing_moved_5yrs, p_hu_vacant), factors = 1),
            immi_con               = rowMeans(cbind(p_lang_eng_limited, p_lang_eng_no))
        )
    # return
    return(d)
})

## save census block group data
write_rds(cook_bg_pv, file.path(PATH, "data", "census-cook-bg-plausible-values.rds"), compress = "gz")

## Joins cb data with bg and ct plausible values
chi_cb     <- read_rds(file.path(PATH, "data", "census-cook-cb-2010.rds"))
cook_bg_pv <- read_rds(file.path(PATH, "data", "census-cook-bg-plausible-values.rds"))
vars_bg <- c("state", "county", "tract", "block_group", "p_occ_management", "p_emp_unemployed",
    "p_poverty", "p_edu_hs", "p_hu_occupied_renter", "p_housing_moved_5yrs", "p_hu_vacant",
    "p_lang_eng_limited", "p_lang_eng_no", "hhinc_median", "hhinc_average", "con_disadv", "res_instab", "immi_con")

trans_bg <- . %>% as_tibble() %>% select(!!quo(vars_bg)) %>% rename_at(vars(-state, -county, -tract, -block_group), funs(paste0(., "_bg")))
bg_data  <- cook_bg_pv %>% map(trans_bg)

chi_cb_pv <- chi_cb %>%
    filter(chicago) %>%
    # spTransform(proj4) %>%
    mutate(
        cb_code                  = paste0(state, county, tract, block),
        bg_code                  = paste0(state, county, tract, block_group),
        crime_violent_2001_rate  = crime_violent_2001/pop,
        crime_property_2001_rate = crime_property_2001/pop,
        crime_violent_2006_rate  = crime_violent_2006/pop,
        crime_property_2006_rate = crime_property_2006/pop,
        crime_violent_2010_rate  = crime_violent_2010/pop,
        crime_property_2010_rate = crime_property_2010/pop,
        major_road               = roads_primary | roads_secondary
    ) %>%
    select(-ends_with("_bg"))
chi_cb_pv <- 1:10 %>% map(~chi_cb_pv %>% left_join(bg_data[[.x]]))

## save census block data
write_rds(chi_cb_pv, file.path(PATH, "data", "census-cook-cb-plausible-values.rds"), compress = "gz")

## quit...
q(save = "no")
