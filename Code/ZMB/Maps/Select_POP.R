#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select population information
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


### LOAD POPULATION MAP
pop_af_2000 <- raster(file.path(dataPath, "Data/Global/WorldPop/Africa-POP-1KM_AFR_PPP_2000_adj_v2/AFR_PPP_2000_adj_v2.tif"))

### LLOAD ADM
adm0 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL_", iso3c_sel, "_adm0_2000.rds")))

### SELECT COUNTRY POPULATION RASTER MAP
pop <- crop(pop_af_2000, adm0)
pop <- mask(pop, adm0)
plot(pop)
hist(pop, breaks = 50)
cellStats(pop,sum)

# Save map
saveRDS(pop, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/pop_", iso3c_sel, ".rds")))

