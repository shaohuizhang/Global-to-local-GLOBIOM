#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to obtain plot area
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


### PLOT AREA
plot_area2010 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_C.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_c00,
            plot_area_qty_f=ag_c04a, plot_area_unit_f=as_factor(ag_c04b), plot_area_qty_gps=ag_c04c)

rm(dataPath)

