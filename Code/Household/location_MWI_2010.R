#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Location of MWI households
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
p_load("haven")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### CREATE LOCATION DF
location2010 <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = as_factor(hh_a01), district_code = hh_a01, rural = as_factor(reside)) %>%
  mutate(rural)

# there are three regions in Malawi:
# 2: Central, 1: Northern and 3: Southern. The first 
# number of the district code tels us which
# is which

location2010$region <- with(location2010,
                             ifelse(district_code < 200, "NORTHERN",
                                    ifelse(district_code >=200 & district_code < 300, "CENTRAL",
                                           "SOUTHERN")))

