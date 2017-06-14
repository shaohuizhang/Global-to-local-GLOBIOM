#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create priors for cross entropy
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


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### LOAD DATA
# Pop per grid cell
tot_pop_grid <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_grid_2000_MWI.csv"))

# ag_stat_2000
ag_stat_2000 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))

### CREATE PRIORS
# Create gridID and crop combinations
crops <- unique(ag_stat_2000$short_name)

prior_base <- expand.grid(gridID = tot_pop_grid$gridID, short_name = crops)

prior <- tot_pop_grid %>%
  mutate(prior = value/sum(value)) %>%
  dplyr::select(gridID, prior) %>%
  left_join(prior_base,.)
