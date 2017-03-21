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

### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


## SURVEY DATA
survey2010 <- read_dta(file.path(dataPath, "Raw/MWI/Household_surveys/2010/IHS3/IHS3_Summary_DTA/ihs3_summary.dta")) %>%
  select(case_id, ea_id, strata, cluster, hhweight)
