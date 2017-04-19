#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to process analyse land cover maps and combine with simu area information
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "plotKML")


### SET WORKING DIRECTORY
wdPath <- "~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD SIMU AREA
simu_area <- read_csv(file.path(dataPath, "Data\\GLOBIOM\\Simu\\Simu_info\\Simu_area.csv"))

### LOAD LAND COVER SHARES
land_cover_shares_2000 <- read_csv(file.path(dataPath, "Data\\Processed\\MWI\\GLOBIOM\\Simu\\Simu_info\\Simu_area.csv"))
