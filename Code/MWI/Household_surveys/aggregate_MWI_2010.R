#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM  
#' Subject:  Script to aggregate household data to admin regions
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
p_load("WDI", "countrycode", "survey")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### WRITE SESSION INFO

### LOAD DATA
MWI2010_imp <- readRDS("Cache/MWI2010_imp.rds")

# AGGREGATE TO HH LEVEL
# Aggregate over plots and crops to household level
MWI2010_hh <- MWI2010_imp %>%
  select(-plotnum, -crop_code) %>%
  group_by(case_id, ea_id, crop) %>%
  summarize_all(funs(mean))
  

# Check if all districts are present
check <- MWI2010_hh %>%
  group_by(cluster, district) %>%
  summarize(n())

# Link location information and weights
suppressMessages(source("Code/Household/location_MWI_2010.R"))
MWI2010_hh <- left_join(MWI2010_hh, location2010)


### AGGREGATE TO THE ADMIN REGION LEVEL
# Aggregate to Admin region level

# Load GADM map
# Note that the island district of Likoma is not covered by the IHS3
MWI_adm1 <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm1.rds"))
MWI_adm1_df <- MWI_adm1@data

# Use weights
# Manually multiply with weight
MWI2010_ag <- MWI2010_hh %>%
  group_by(district, crop) %>%
  summarize(qty = sum(crop_qty_harv*hhweight, na.rm=T)) %>%
  spread(crop, qty) 

sum(MWI2010_ag$SORGHUM, na.rm=T)/1000
sum(MWI2010_ag$IRISH_[MALAWI]_POTATO, na.rm=T)/1000

names(MWI2010_ag)
# Map data

# 

### Save file

