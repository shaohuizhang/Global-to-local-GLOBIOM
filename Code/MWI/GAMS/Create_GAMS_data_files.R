#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create GAMS sets
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
p_load("countrycode")


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


### LOAD DATA
# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_2000_MWI.rds"))

# Agricultural statistics
ag_stat_2000 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))

# Gridded adm
adm_grid_2000 <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv"))


### CREATE GAMS INPUT DATA FILES
# CHECK: WILL BE REPLACED BY SCRIPT THAT WRITES GDX

# deptots(k,s)
deptots <- ag_stat_2000 %>%
  filter(adm_level == 2) %>%
  select(adm, short_name, value)

write_csv(deptots, file.path(dataPath, "Model/Data/deptots.csv"), col_names = F)

# avail(i)
avail <- crop_cover %>%
  select(gridID, area)

write_csv(avail, file.path(dataPath, "Model/Data/avail.csv"), col_names = F)

# produ(j)
produ <- ag_stat_2000 %>%
  filter(adm_level == 0) %>%
  select(short_name, value)

write_csv(produ, file.path(dataPath, "Model/Data/produ.csv"), col_names = F)
