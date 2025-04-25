
# working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
# load libraries and functions
library("stringr")
library("tidyverse")
library("sp")
library("spdplyr")
library("lubridate")
library("httr")
library("car", pos = length(search()))
# utility functions
source(file.path(PATH, "00-utils.r"))
options("scipen" = 10000)
# Coordinate reference system
proj4_longlat <- CRS("+proj=longlat +ellps=GRS80")

## Chicago Open Data - SODA API endpoints
# https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
# Description: This dataset reflects reported incidents of crime (with the exception of murders where data exists for each victim) that occurred in the City of Chicago from 2001 to present, minus the most recent seven days. Data is extracted from the Chicago Police Department's CLEAR (Citizen Law Enforcement Analysis and Reporting) system. In order to protect the privacy of crime victims, addresses are shown at the block level only and specific locations are not identified. Should you have questions about this dataset, you may contact the Research & Development Division of the Chicago Police Department at 312.745.6071 or RandD@chicagopolice.org. Disclaimer: These crimes may be based upon preliminary information supplied to the Police Department by the reporting parties that have not been verified. The preliminary crime classifications may be changed at a later date based upon additional investigation and there is always the possibility of mechanical or human error. Therefore, the Chicago Police Department does not guarantee (either expressed or implied) the accuracy, completeness, timeliness, or correct sequencing of the information and the information should not be used for comparison purposes over time. The Chicago Police Department will not be responsible for any error or omission, or for the use of, or the results obtained from the use of this information. All data visualizations on maps should be considered approximate and attempts to derive specific addresses are strictly prohibited. The Chicago Police Department is not responsible for the content of any off-site pages that are referenced by or that reference this web page other than an official City of Chicago or Chicago Police Department web page. The user specifically acknowledges that the Chicago Police Department is not responsible for any defamatory, offensive, misleading, or illegal conduct of other users, links, or third parties and that the risk of injury from the foregoing rests entirely with the user. The unauthorized use of the words "Chicago Police Department," "Chicago Police," or any colorable imitation of these words or the unauthorized use of the Chicago Police Department logo is unlawful. This web page does not, in any way, authorize such use. Data is updated daily Tuesday through Sunday. The dataset contains more than 65,000 records/rows of data and cannot be viewed in full in Microsoft Excel. Therefore, when downloading the file, select CSV from the Export menu. Open the file in an ASCII text editor, such as Wordpad, to view and search. To access a list of Chicago Police Department - Illinois Uniform Crime Reporting (IUCR) codes, go to http://data.cityofchicago.org/Public-Safety/Chicago-Police-Department-Illinois-Uniform-Crime-R/c7ck-438e
app_token <- 'YOUR TOKEN'
N         <- 6463522
limit     <- 1000
limit     <- 50000
k         <- ceiling(N/limit)

## Call API to get crime data
# httr::GET(url, query = list('$limit' = 1000, '$offset' = 0)) %>% httr::content(col_types = col_spec)
url       <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv"
chi_crime <- 1:k %>%
    map(~list('$limit' = limit, '$offset' = (.x-1)*limit, '$$app_token' = app_token)) %>%
    map(~httr::GET(url, query = .x))
# check response code
chi_crime %>% map_int(httr::status_code) %>% table()
# combine responses
col_spec <- cols(.default = col_character(), ID = col_integer(), Ward = col_integer(), `Community Area` = col_integer(), `X Coordinate` = col_integer(), `Y Coordinate` = col_integer(), Year = col_integer(), Latitude = col_double(), Longitude = col_double())
chi_crime <- chi_crime %>%
    # keep(~httr::status_code(.x) == 200) %>%
    map(httr::content, col_types = col_spec) %>%
    bind_rows()
# save response
write_rds(chi_crime, file.path(PATH, "data", "chi-crime-2001-2016.rds"), compress = "gz")

## Read TIGER shapefile for cook county
cook_cb_2000 <- read_rds(file.path(PATH, "data", "shapefiles", "cook-census-block-TIGER-2000.rds"))
cook_cb_2010 <- read_rds(file.path(PATH, "data", "shapefiles", "cook-census-block-TIGER-2010.rds"))

## Recode Chicago crime data
chi_crime  <- read_rds(file.path(PATH, "data", "chi-crime-2001-2016.rds"))
# UCR part I violent crimes: murder, manslaughter, robbery, and aggravated assault (forcible rape is excluded)
# (1) criminal homicide (murder and nonnegligent manslaughter, manslaughter by negligence)
# (2) forcible rape (rape by force, attempt to commit forcible rape)
# (3) robbery (firearm, knife or cutting instrument, other dangerous weapon, strong-arm—hands, fists, feet, etc.)
# (4) (aggravated) assault (firearm, knife or cutting instrument, other dangerous weapon, hands, fists, feet, etc.—aggravated injury, simple, not aggravated)
# '0142' - RECKLESS HOMICIDE?
homicide       <- c("0110", "0130", "0141")
robbery        <- c("0312", "0313", "0320", "0325", "0326", "0330", "0331", "0334", "0337", "0340")
sexual_assault <- c("0261", "0262", "0263", "0264", "0265", "0271", "0272", "0273", "0274", "0275")
agg_assault    <- c("0520", "0530", "0550", "0551", "0552", "0553", "0554", "0555", "0556", "0557", "0558")
iucr_violent   <- c(homicide, robbery, sexual_assault, agg_assault)
# UCR part I property crimes: burglary, larceny/theft?, motor vehicle theft, and arson
burglary      <- c("0610", "0620", "0630", "0650")
theft         <- c("0810", "0820", "0840", "0841", "0842", "0843", "0850", "0860", "0865", "0870", "0880", "0890", "0895")
mvtheft       <- c("0910", "0915", "0917", "0918", "0920", "0925", "0927", "0928", "0930", "0935", "0937", "0938")
arson         <- c("1010", "1020", "1025", "1030", "1035", "1090")
iucr_property <- c(burglary, theft, mvtheft, arson)
# recode data
names(chi_crime) <- tolower(names(chi_crime))
chi_crime <- chi_crime %>%
    dplyr::transmute(
        id           = id,
        # http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones
        year         = year,
        time         = as.POSIXct(date, format = "%m/%d/%Y %I:%M:%S %p", tz = 'America/Chicago'),
        date         = as.Date(time),
        domestic     = car::recode(domestic, "'false'=FALSE; 'true'=TRUE"),
        arrest       = car::recode(arrest, "'false'=FALSE; 'true'=TRUE"),
        iucr         = str_digits(iucr, 4),
        homicide     = iucr %in% homicide,
        ucr_violent  = iucr %in% iucr_violent,
        ucr_property = iucr %in% iucr_property,
        primary_type = `primary type`,
        description  = description,
        fbi_code     = `fbi code`,
        xcoord       = `x coordinate`,
        ycoord       = `y coordinate`,
        latitude     = latitude,
        longitude    = longitude) %>%
    # filter: 0.8% removed because of missing data
    filter(complete.cases(time, latitude, longitude))

# chicago: get coordinates
coord <- select(chi_crime, longitude, latitude)
coordinates(coord) <- ~ longitude + latitude
proj4string(coord) <- proj4_longlat
chi_crime <- over(coord, cook_cb_2000) %>%
    select(state, county, tract_2000 = tract, block_2000 = block) %>%
    bind_cols(chi_crime) %>%
    select(id, year, everything())
chi_crime <- over(coord, cook_cb_2010) %>%
    select(tract_2010 = tract, block_2010 = block) %>%
    bind_cols(chi_crime) %>%
    select(id, year, state, county, tract_2000, block_2000, tract_2010, block_2010, everything())

## save crime data
write_rds(chi_crime, file.path(PATH, "data", "chi-crime-2001-2016-recoded.rds"), compress = "gz")

## Aggregate to census block, block group and tract level
summarize_crime <- . %>% summarize(
        crime          = n(),
        crime_homicide = sum(homicide),
        crime_violent  = sum(ucr_violent),
        crime_property = sum(ucr_property),
        # night and daytime
        crime_night          = sum(!daytime),
        crime_homicide_night = sum(homicide & !daytime),
        crime_violent_night  = sum(ucr_violent & !daytime),
        crime_property_night = sum(ucr_property & !daytime),
        crime_day            = sum(daytime),
        crime_homicide_day   = sum(homicide & daytime),
        crime_violent_day    = sum(ucr_violent & daytime),
        crime_property_day   = sum(ucr_property & daytime),
    )
# night and daytime
day_begin <- ymd_hms("2017-11-01 06:00:00", tz = "America/Chicago")
day_end   <- ymd_hms("2017-11-01 17:59:59", tz = "America/Chicago")
chi_crime <- chi_crime %>%
    mutate(daytime = time %>% str_replace("\\d{4}-\\d{2}-\\d{2}", "2017-11-01") %>% ymd_hms(tz = "America/Chicago") %>% between(day_begin, day_end))
# census block
chi_crime_cb <- chi_crime %>%
    rename(tract = tract_2010, block = block_2010) %>%
    group_by(state, county, tract, block, year) %>%
    summarize_crime()
## save data
write_rds(chi_crime_cb, file.path(PATH, "data", "chi-crime-cb-2001-2016.rds"), compress = "gz")

## quit...
q(save = "no")
