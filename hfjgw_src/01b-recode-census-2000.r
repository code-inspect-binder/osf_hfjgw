
# set working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
## load libraries
library("stringr")
library("tidyverse")
library("sp")
library("spdplyr")
library("lubridate")
library("matrixStats")
library("rgeos")
# utility functions
source(file.path(PATH, "00-utils.r"))
source(file.path(PATH, "areal_wombling.r"))

## Chicago census blocks and block groups
# 15-digit identifier for census blocks, block groups (12 digits), and tract (11 digits) based on fips code
chi_cb           <- open_shapefile(file.path(PATH, "data", "shapefiles", "chicago-cb-2000.zip"))
chi_blocks       <- chi_cb$census_b_1 %>% as.character()
chi_block_groups <- chi_blocks %>% str_sub(1, 12) %>% unique()

## Census data (census block)
# download census data for cook county
# cook_cb_sf1 <- censusr::census_api(geography = "block", filter = "17031", data = "census_sf1", year = 2000)
# write_rds(cook_cb_sf1, file.path(PATH, "data", "cook-census-block-sf1-2000.rds"), compress = "gz")

# Read Cook county 2000 TIGER shapefile (fips code "031")
cook_cb <- read_rds(file.path(PATH, "data", "shapefiles", "cook-census-block-TIGER-2000.rds"))
cook_cb <- cook_cb %>%
    filter(county == "031") %>%
    mutate(chicago = str_c(state, county, tract, block) %in% chi_blocks)
# Read census data
cook_cb_sf1 <- read_rds(file.path(PATH, "data", "cook-census-block-sf1-2000.rds"))
# Diversity, concentrated disad., resid. instab., immi. concentration
cook_cb_sf1 <- cook_cb_sf1 %>%
    mutate(
        hhi   = f_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other)),
        epol  = p_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other)),
        theil = th_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other))
    )
# Join shapefile with census data
vars_by <- c("state", "county", "tract", "block")
cook_cb@data <- cook_cb@data %>% left_join(cook_cb_sf1, by = vars_by)

## Census data (census block group)
# download census data for cook county with censusr package 
# x <- c("default", "P077001", "P078001", "P010006", "P082001")
# cook_bg_sf1 <- censusr::census_api(geography = "block_group", filter = "17031", data = "census_sf1", year = 2000)
# cook_bg_sf3 <- censusr::census_api(geography = "block_group", filter = "17031", data = "census_sf3", year = 2000)
# write_rds(cook_bg_sf1, file.path(PATH, "data", "cook-block-group-sf1-2000.rds"), compress = "gz")
# write_rds(cook_bg_sf3, file.path(PATH, "data", "cook-block-group-sf3-2000.rds"), compress = "gz")

# Read census data
cook_bg_sf1 <- read_rds(file.path(PATH, "data", "cook-block-group-sf1-2000.rds"))
cook_bg_sf3 <- read_rds(file.path(PATH, "data", "cook-block-group-sf3-2000.rds"))
# select acs variables not in summary file 1
cook_bg_sf3 <- cook_bg_sf3 %>% select_(.dots = names(cook_bg_sf3)[!names(cook_bg_sf3) %in% names(cook_bg_sf1)[-1:-4]])
# join shapefile with census data
vars_by <- c("state", "county", "tract", "block_group")
cook_bg <- cook_bg_sf1 %>% left_join(cook_bg_sf3, by = vars_by)
# Diversity, concentrated disad., resid. instab., immi. concentration
cook_bg <- cook_bg %>%
    mutate(
        hhi        = f_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other)),
        epol       = p_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other)),
        theil      = th_index(cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other)),
        con_disadv = fac(cbind(p_occ_management, p_emp_unemployed, p_poverty, p_edu_hs, p_single_mother), factors=1),
        res_instab = fac(cbind(p_hu_occupied_renter, p_housing_moved_5yrs, p_hu_vacant), factors=1),
        immi_con   = rowMeans(cbind(p_lang_eng_limited, p_lang_eng_no))
    )

## Areal wombling
vars <- c("p_race_white", "p_race_black", "p_race_hisp", "p_race_asian")
summarise_blv <- . %>% summarise(
    p_race_white_blv = max(p_race_white_blv, na.rm = TRUE),
    p_race_white_bmv = max(p_race_white_bmv, na.rm = TRUE),
    p_race_black_blv = max(p_race_black_blv, na.rm = TRUE),
    p_race_black_bmv = max(p_race_black_bmv, na.rm = TRUE),
    p_race_hisp_blv  = max(p_race_hisp_blv, na.rm = TRUE),
    p_race_hisp_bmv  = max(p_race_hisp_bmv, na.rm = TRUE),
    p_race_asian_blv = max(p_race_asian_blv, na.rm = TRUE),
    p_race_asian_bmv = max(p_race_asian_bmv, na.rm = TRUE))
sl_cb <- areal_wombling(cook_cb, vars)
# aggregate from line segments to block group level
chi_cb_blv <- 
    bind_rows(
        group_by(sl_cb@data, i) %>% summarise_blv(),
        group_by(sl_cb@data, j) %>% summarise_blv() %>% rename(i = j)
    ) %>%
    group_by(i) %>%
    summarise_blv()
cook_cb@data <- bind_cols(cook_cb@data, select(chi_cb_blv, -i))
# combine racial edges
cook_cb$edge_race_areal <- with(cook_cb@data, matrixStats::rowMaxs(cbind(p_race_white_blv, p_race_black_blv, p_race_hisp_blv, p_race_asian_blv), na.rm = FALSE))

## save census data
write_rds(cook_cb, file.path(PATH, "data", "census-cook-cb-2000.rds"), compress = "gz")

## Spatial lag terms
mutate_spatial_lag <- . %>% mutate(
        p_white_splag    = p_race_white %>% spatial_lag(W = W_dist),
        p_black_splag    = p_race_black %>% spatial_lag(W = W_dist),
        p_asian_splag    = p_race_asian %>% spatial_lag(W = W_dist),
        p_hisp_splag     = p_race_hisp %>% spatial_lag(W = W_dist),
        hhi_splag        = hhi %>% spatial_lag(W = W_dist),
        p_white_splag_nb = p_race_white %>% spatial_lag(W = W_neigh),
        p_black_splag_nb = p_race_black %>% spatial_lag(W = W_neigh),
        p_asian_splag_nb = p_race_asian %>% spatial_lag(W = W_neigh),
        p_hisp_splag_nb  = p_race_hisp %>% spatial_lag(W = W_neigh),
        hhi_splag_nb     = hhi %>% spatial_lag(W = W_neigh)
    )
# census block
W_dist  <- spatial_weight(cook_cb, km = 4, sparse_matrix = FALSE)
W_neigh <- spatial_weight_neighbors(cook_cb, sparse_matrix = FALSE)
cook_cb@data <- cook_cb@data %>% mutate_spatial_lag()

## Joins cb with bg data
vars <- names(cook_bg) %>% discard(`%in%`, names(cook_cb)[-1:-4])
bg_data <- cook_bg %>%
    select(!!quo(vars)) %>%
    select(-ends_with("_moe")) %>%
    rename_at(vars(-state, -county, -tract, -block_group), funs(paste0(., "_bg")))
cook_cb@data <- cook_cb@data %>%
    mutate(block_group = str_sub(block, 1, 1)) %>%
    dplyr::left_join(bg_data)

## save census data
write_rds(cook_cb, file.path(PATH, "data", "census-cook-cb-2000.rds"), compress = "gz")

## quit...
q(save = "no")
