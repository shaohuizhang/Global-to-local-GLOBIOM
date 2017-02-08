#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to prepare crop level dataframe
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
# wdPath<-"~/Global-to-local-GLOBIOM"
# setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### PLANTED AREA AND CROP HARVESTED (BOTH HH TOTAL, NOT SPECIFIED BY PLOT)
# load crop codes
crop_list <- read_csv(file.path(dataPath, "Conversion/MWI_crop_list_2010.csv"))

# Get data
crop_hh2010 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_B.dta")) %>%
  transmute(case_id, ea_id, crop_code=as.integer(ag_b0c), planted_area_qty_f=ag_b01a, planted_area_unit_f=ag_b01b,
            crop_qty_harv=ag_b04a, crop_qty_unit=ag_b04b, crop_qty_condition=ag_b04c) %>%
            left_join(., crop_list)

# take out the trash
rm(list=c("dataPath"))

