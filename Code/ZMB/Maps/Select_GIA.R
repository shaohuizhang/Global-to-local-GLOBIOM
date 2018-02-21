#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select Global Irrigated Areas data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if (!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("quickPlot")


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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD DATA
# adm
adm <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL/adm_2000_", iso3c_sel, ".rds")))

# gia
gia_raw <- raster(file.path(dataPath, "Data/global/gia/global_irrigated_areas.tif"))
crs(gia_raw) <- crs(adm)


### PROCESS
# Select country map
gia <- crop(gia_raw, adm)
gia <- mask(gia, adm)
names(gia) <- "gia"

# Remove cells with 0 and add area info
gia[gia == 0] <- NA # set non-irrigated areas to NA
gia_area <- area(gia)
gia_area <- gia_area*100 # to ha
gia_area[is.na(gia)] <- NA
names(gia_area) <- "value"
rm(gia)

levelplot(gia_area) +
  layer(sp.polygons(adm, col = "black"))
freq(gia_area)

# Save map
giaPath <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gia"))
dir.create(giaPath)
writeRaster(gia_area, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gia/gia_", iso3c_sel, ".tif")), overwrite = T)


