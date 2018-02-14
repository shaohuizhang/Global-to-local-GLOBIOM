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


### SELECT COUNTRY GIA MAP
gia <- crop(gia_raw, adm)
gia <- mask(gia, adm)
names(gia) <- "gia"
gia[gia == 0] <- NA # set non-irrigated areas to NA
levelplot(gia) +
  layer(sp.polygons(adm, col = "black"))
freq(gia)


# Save map
giaPath <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gia"))
dir.create(giaPath)
writeRaster(gia, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gia/gia_", iso3c_sel, ".tif")), overwrite = T)


