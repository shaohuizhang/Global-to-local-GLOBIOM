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


### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### PLOT AREA
plot_area2010 <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Agriculture/AG_MOD_C.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_c00,
            plot_area_qty_f=ag_c04a, plot_area_unit_f=as_factor(ag_c04b), plot_area_qty_gps=ag_c04c)


