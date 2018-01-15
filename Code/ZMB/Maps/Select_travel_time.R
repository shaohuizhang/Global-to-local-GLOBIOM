#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select travel time information
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD TRAVEL TIME MAP
tt_raw <- "P:\\d4ca\\Data Resources\\ACCESS_50K\\access_50k/acc_50k"
tt_raw <- raster(file.path(dataPath, "Data\\Global\\ACCESS_50K\\access_50k/acc_50k"))

### LLOAD ADM
adm0 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/gaul/GAUL_", iso3c_sel, "_adm0_2000.rds")))

### SELECT COUNTRY GMIA RASTER MAP
tt <- crop(tt_raw, adm0)
tt <- mask(tt, adm0)
plot(tt)
plot(adm0, add = T)

# Save map
travel_timePath <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\travel_time"))
dir.create(travel_timePath)

writeRaster(tt, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/travel_time_", iso3c_sel, ".tif")), overwrite = T)

