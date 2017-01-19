#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### ETH AND TZA MAPS
# Download
ETH_adm1 <- readRDS("Data/GADM_2.8_ETH_adm1.rds")
TZA_adm1 <- readRDS("Data/GADM_2.8_TZA_adm1.rds")

ETH_adm <- spplot(ETH_adm1, "OBJECTID", main = "Regional states (Kililoch) in Ethiopia", sub = "Source: www.GADM.org", colorkey=FALSE)
TZA_adm <- spplot(TZA_adm1, "OBJECTID", main = "Regions (mkoa) in Ethiopia", sub = "Source: www.GADM.org", colorkey=FALSE)
