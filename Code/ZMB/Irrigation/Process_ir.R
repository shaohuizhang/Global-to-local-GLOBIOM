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
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
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
ir_crop_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/aquastat_ir_crops_", iso3c_sel, ".csv"))) 

# GMIA
gmia <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gmia/GMIA_30sec_", iso3c_sel, ".tif")))


### COMPARE TOTALS BETWEEN SOURCES
# GMIA area equipped for irrigation
cellStats(gmia_5min, sum)

# Aquastat area equipped for irrigation
ir_crop_raw[ir_crop_raw$variable == "Area equipped for irrigation",]

# Aquastat total harvested area full control irrigated
ir_crop_raw[ir_crop_raw$variable == "Total harvested irrigated crop area (full control irrigation)",]


### SELECT DATA
# NB: Depending on country select tropical (trof) or temperate fruits for "Other fruits" category if available
# NB: fodder and Grass and fodder are not mapped

# We select 2002 data to represent 2000
ir_crop_2000 <- filter(ir_crop_raw, short_name != "total", year == 2002) %>%
  dplyr::select(-variable) %>%
  dplyr::rename(adm = iso3c) %>%
    mutate(system = "I", 
           adm_level = 0)
sum(ir_crop_2000$value)

# Save
write_csv(ir_crop_2000, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ir_2000_", iso3c_sel, ".csv")))

