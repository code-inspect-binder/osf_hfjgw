
# working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
## load libraries
library("MASS")
library("tidyverse")
library("readr")
library("stargazer")
library("spdep")
library("purrr")
library("MASS")
library("spdplyr")
library("rgeos")
library("maptools")
library("car", pos = length(search()))
# utility functions
proj4  <- CRS(" +proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
source(file.path(PATH, "00-utils.r"))
source(file.path(PATH, "areal_wombling.r"))
# read data
chi_cb <- read_rds(file.path(PATH, "data", "census-cook-cb-2010.rds"))
chi_cb <- chi_cb %>%
    filter(chicago) %>%
    spTransform(proj4) %>%
    mutate(
        rowid   = row.names(.),
        cb_code = paste0(state, county, tract, block),
        bg_code = paste0(state, county, tract, block_group)
    )
chi_cb <- chi_cb[-28816,]

## Parks
chi_parks <- rgdal::readOGR(path.expand(file.path(PATH, "data", "shapefiles", "chicago-parks-2012")), "Parks_Aug2012")
chi_parks <- chi_parks %>% spTransform(proj4) %>% unionSpatialPolygons(IDs = rep(1, length(chi_parks)))
# plot(chi_parks, col = "#90c09050", bor = "#ffffff00")
# Difference between census blocks and parks
cb_parks_diff <- rgeos::gDifference(chi_cb, chi_parks, byid = TRUE)
row.names(cb_parks_diff) <- row.names(cb_parks_diff) %>% str_replace(" 1", "")
cb_parks_area <- tibble(
        rowid        = row.names(cb_parks_diff),
        area_notpark = gArea(cb_parks_diff, byid = TRUE),
    ) 
chi_cb@data <- chi_cb@data %>%
    left_join(cb_parks_area, by = "rowid") %>%
    mutate(
        area         = gArea(chi_cb, byid = TRUE) %>% unname(),
        area_notpark = coalesce(area_notpark, area),
        p_park       = (area - area_notpark)/area,
        park_area    = p_park > 0.5,
    )
# Adjacent to park
chi_cb_parks <- chi_cb %>%
    filter(park_area) %>%
    select(state, county, tract, block, cb_code, p_park) %>%
    unionSpatialPolygons(IDs = rep(1, length(.)))
chi_cb$touches_park <- rgeos::gTouches(chi_cb, chi_cb_parks, byid = TRUE)[1,]

## Topological Faces (Polygons with All Geocodes)
# Download: ftp://ftp2.census.gov/geo/tiger/TIGER2010/FACES/tl_2010_17031_faces.zip
# Documentation: http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2010/TGRSHP10SF1.pdf (Section 5.20)
chi_faces <- rgdal::readOGR(path.expand(file.path(PATH, "data", "shapefiles", "chicago-faces-2010")), "tl_2010_17031_faces")
chi_faces <- chi_faces@data %>%
    transmute(
        id_face     = TFID,
        state       = STATEFP10,
        county      = COUNTYFP10,
        tract       = TRACTCE10,
        block_group = BLKGRPCE10,
        block       = BLOCKCE10,
        schooldist_elementary = ELSDLEA10,
        schooldist_secondary  = SCSDLEA10,
        schooldist_unified    = UNSDLEA10
    ) %>%
    mutate_if(is.factor, as.character) %>%
    as_tibble()

## Linear Features
# Download: https://www.census.gov/geo/maps-data/data/tiger-line.html
# Documentation: http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2010/TGRSHP10SF1.pdf (Section 5.11)
chi_lines <- rgdal::readOGR(path.expand(file.path(PATH, "data", "shapefiles", "chicago-lines-2010")), "tl_2010_17031_edges")
chi_lines <- chi_lines@data %>% transmute(id_edge = TLID, id_face_left = TFIDL, id_face_right = TFIDR, fullname = FULLNAME, MTFCC = MTFCC)
# join lines with geo-codes of left and right faces 
chi_lines <- chi_lines %>%
    left_join(chi_faces, by = c('id_face_left' = 'id_face')) %>%
    left_join(select(chi_faces, -state, -county), by = c('id_face_right' = 'id_face'), suffix = c("_left", "_right")) %>%
    mutate(
        cb_code_left = str_c(state, county, tract_left, block_left),
        cb_code_right = str_c(state, county, tract_right, block_right)
    ) %>%
    select(-state, -county, -tract_left, -block_group_left, -block_left, -tract_right, -block_group_right, -block_right)

## Primary and Secondary roads (Highways)
# Primary roads (MTFCC = "S1100") are generally divided, limited-access highways within the Federal interstate highway system or under state management, and are distinguished by the presence of interchanges. These highways are accessible by ramps and may include some toll highways.
# Secondary roads (MTFCC = "S1200") are main arteries, usually in the U.S. Highway, State Highway or County Highway system. These roads have one or more lanes of traffic in each direction, may or may not be divided, and usually have at-grade intersections with many other roads and driveways. They often have both a local name and a route number.
chi_roads <- chi_lines %>% filter(MTFCC %in% c("S1100", "S1200"))
# census block codes for primary and secondary roads
chi_roads_primary <- chi_roads %>%
    filter(MTFCC == "S1100") %>%
    select(id_edge, cb_code_left, cb_code_right) %>%
    tidyr::gather(side, cb_code, cb_code_left, cb_code_right) %>%
    pull("cb_code") %>%
    unique() %>%
    na.omit()
chi_roads_secondary <- chi_roads %>%
    filter(MTFCC == "S1200") %>%
    select(id_edge, cb_code_left, cb_code_right) %>%
    tidyr::gather(side, cb_code, cb_code_left, cb_code_right) %>%
    pull("cb_code") %>%
    unique() %>%
    na.omit()
# add covariates to census block file
chi_cb <- chi_cb %>%
    mutate(
        roads_primary   = cb_code %in% chi_roads_primary,
        roads_secondary = cb_code %in% chi_roads_secondary,
    )

## Rivers
# Stream/River (MTFCC = "H3010") A natural flowing waterway. [includes anabranch, awawa, branch, brook, creek, distributary, fork, kill, pup, rio, and run]
chi_rivers <- chi_lines %>% filter(MTFCC %in% c("H3010"))
# census block codes for primary and secondary roads
chi_rivers_cb <- chi_rivers %>%
    select(id_edge, cb_code_left, cb_code_right) %>%
    tidyr::gather(side, cb_code, cb_code_left, cb_code_right) %>%
    pull("cb_code") %>%
    unique() %>%
    na.omit()
# add covariates to census block file
chi_cb <- chi_cb %>% mutate(river = cb_code %in% chi_rivers_cb)

## Distance to cps elementary school district
chi_cps_elementary <- rgdal::readOGR(path.expand(file.path(PATH, "data", "shapefiles", "cps-elementary-school-boundaries")), "cps-elementary-school-boundaries")
# school district: project and convert to spatial lines
chi_cps_elementary <- chi_cps_elementary %>% spTransform(proj4) %>% border_lines()
# census blocks: convert to spatial lines
chi_cb_sl       <- border_lines(chi_cb)
# school district: select valid spatial lines
sel_elementary <- (1:nrow(chi_cps_elementary) %>% map_dbl(~possibly(gDistance, otherwise = -9999)(chi_cb_sl[1,], chi_cps_elementary[.x,], byid = TRUE)[1])) != -9999
# census blocks: select valid spatial lines
sel_cb_sl <- 1:nrow(chi_cb_sl) %>%
    map(~possibly(gDistance, otherwise = NULL)(chi_cb_sl[.x,], chi_cps_elementary[sel_elementary,], byid = TRUE)) %>%
    map_lgl(is.matrix)
write_rds(sel_cb_sl, file.path(PATH, "data", "select-cb-sl-cps-elementary.rds"), compress = "gz")
# sel_cb_sl <- read_rds(file.path(PATH, "data", "select-cb-sl-cps-elementary.rds"))

# distance between census blocks and school district
dist_cb_cps_elementary <- rgeos::gDistance(chi_cb_sl[sel_cb_sl,], chi_cps_elementary[sel_elementary,], byid = TRUE)
write_rds(dist_cb_cps_elementary, file.path(PATH, "data", "dist-cb-cps-elementary.rds"), compress = "gz")
# dist_cb_cps_elementary <- read_rds(file.path(PATH, "data", "dist-cb-cps-elementary.rds"))
dist_cb_cps_elementary <- dist_cb_cps_elementary %>% apply(2, min)
# aggregate distance to census block level
chi_cb_sl_dist <- tibble(
        idx = names(dist_cb_cps_elementary),
        dist = unname(dist_cb_cps_elementary)
    ) %>%
    separate(idx, into = c("i", "j"), sep = "_") %>%
    mutate(
        i = str_replace(i, "i", "") %>% as.integer(),
        j = str_replace(j, "j", "") %>% as.integer(),
    )
dist_cps_elementary <- bind_rows(
        chi_cb_sl_dist %>1% group_by(i) %>% summarize(dist = min(dist)),
        chi_cb_sl_dist %>% group_by(j) %>% summarize(dist = min(dist)) %>% rename(i = j)
    ) %>%
    group_by(i) %>%
    summarize(dist = min(dist))
write_rds(dist_cps_elementary, file.path(PATH, "data", "dist-cb-cps-elementary-reduced.rds"), compress = "gz")
dist_cps_elementary <- read_rds(file.path(PATH, "data", "dist-cb-cps-elementary-reduced.rds"))
# join distance with census block file
chi_cb@data <- chi_cb@data %>%
    mutate(i = 1:nrow(.)) %>%
    left_join(dist_cps_elementary) %>%
    rename(dist_cps_elementary = dist) %>%
    mutate(elementary_school_district = dist_cps_elementary %in% 0)

## save census data
write_rds(chi_cb, file.path(PATH, "data", "census-cook-cb-2010.rds"), compress = "gz")

## quit...
q(save = "no")
