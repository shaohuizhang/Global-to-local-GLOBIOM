#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to combine irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "imputeTS")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# Aquastat
ir_crop_raw <- read_csv(file.path(dataPath, "Data/Global/AQUASTAT/Irrigated_area_by_crop.csv")) %>%
  filter(iso3c == iso3c_sel)

# GMIA
GMIA <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GMIA_", iso3c_sel, ".rds")))


### COMPARE TOTALS BETWEEN SOURCES
# GMIA area equipped for irrigation
cellStats(GMIA,sum)

# Aquastat area equipped for irrigation
ir_crop_raw[ir_crop_raw$variable == "Area equipped for irrigation",]

# Aquastat total harvested area irrigated
ir_crop_raw[ir_crop_raw$variable == "Total harvested irrigated crop area (full control irrigation)",]


