#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM  
#' Subject:  Script to collect survey data (e.g. clusters and weights)
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
#p_load("WDI", "countrycode", "survey")


### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


## SURVEY DATA
survey2010 <- read_dta(file.path(dataPath, "IHS3_Summary_DTA/ihs3_summary.dta")) %>%
  select(case_id, ea_id, strata, cluster, hhweight)
