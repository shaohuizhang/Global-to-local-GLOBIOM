#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Test
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "haven")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")


### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### PERMANENT CROPS
# crop output
pcrops2010 <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Agriculture/AG_MOD_P.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_p0b, crop_code=as_factor(ag_p0d),
         area_qty=ag_p02a, area_unit=as_factor(ag_p02b), 
         crop_qty_harv=ag_p09a, crop_qty_unit=as_factor(ag_p09b))

# get region variable
region <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = hh_a01)
region$region <- with(region,
                      ifelse(district < 200, 1,
                             ifelse(district >=200 & district < 300, 2,
                                    3)))
region$district <- NULL
region$region <- as.integer(region$region)

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Conversion/IHS.Agricultural.Conversion.Factor.Database.dta")) %>%
  transmute(crop_code = as_factor(crop_code), unit = toupper(as_factor(unit)), condition = toupper(as_factor(condition)), region = as.integer(region), 
            shell_unshelled, conversion)

# It appears that the conversion file does not contain information on permanent crops. 
# As we only need the location of the crop, this is not a problem. 
# We do not convert.

# if someone either did not have any output, no crop code or
# had 0 output remove them from the data frame
pcrops2010 <- pcrops2010 %>%
  filter(!is.na(crop_code), !is.na(crop_qty_harv), !(crop_qty_harv %in% 0))

# take out the trash
rm(list=c("dataPath", "region", "qty2kg"))
