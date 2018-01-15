#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to combine different land cover maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("countrycode", "plotKML")

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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### SET COUNTRY
source("Code/MWI/Set_country.R")

### DATA
# ESA Land cover
esa_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_ESA_2000_", iso3c_sel, ".rds")))

# FAO land cover
fao_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_FAO_2000_", iso3c_sel, ".rds")))

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"


### COMBINE MAPS
fao <- fao_raw %>%
  dplyr::select(gridID, lc_fao = lc, adm2, lc_area)

esa <- esa_raw %>%
  dplyr::select(gridID, lc_esa = lc, adm2, lc_area)

lc <- full_join(esa, fao) %>%
  mutate(comp = ifelse(lc_esa == "other" | lc_fao == "other" | is.na(lc_fao) | is.na(lc_esa), 
                      "out", "in")) %>%
  filter(comp == "in")


# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_2000_", iso3c_sel, ".rds")))
