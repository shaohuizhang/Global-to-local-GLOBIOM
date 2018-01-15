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

# Crop land mask
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/processed/Spatial_data/crop_cover_2000_MWI.rds"))


### CREATE PRIORS
# Calculate prior for crop area by using share of rural population. 
# If we have information on crop area at adm2 level, we calculate the prior at adm2 level.

# Create gridID and crop combinations
crops <- unique(ag_stat_2000$short_name)
priors_base <- expand.grid(gridID = tot_pop_grid$gridID, short_name = crops)

# Merge pop and crop cover data to select relevant grid cells
# Remove grid cells with no crop area
grid_sel <- left_join(crop_cover, tot_pop_grid) %>%
  filter(area > 0)

# Calculate crop area prior for crops at adm2 level
crop_area_prior_adm2 <- grid_sel %>%
  group_by(adm2) %>%
  mutate(pop_share = value/sum(value)) %>%
  dplyr::select(gridID, adm = adm2, pop_share) %>%
  left_join(.,ag_stat_2000) %>%
  mutate(crop_area_prior = pop_share * value) %>%
  dplyr::select(gridID, adm, short_name, crop_area_prior)

# Calculate crop area prior for crops at adm0 level
# Need to filter out crops for which adm2 data is available
crops_adm2 <- unique(crop_area_prior_adm2$short_name)
ag_stat_2000_adm0 <- filter(ag_stat_2000, !(short_name %in% crops_adm2))

crop_area_prior_adm0 <- grid_sel %>%
  group_by(adm0) %>%
  mutate(pop_share = value/sum(value)) %>%
  dplyr::select(gridID, adm = adm0, pop_share) %>%
  left_join(., ag_stat_2000_adm0) %>%
  mutate(crop_area_prior = pop_share * value) %>%
  dplyr::select(gridID, adm, short_name, crop_area_prior)

# Calculate prior as share of total
# Apply crop_cover
prior <- bind_rows(crop_area_prior_adm0, crop_area_prior_adm2) %>%
  ungroup() %>%
  group_by(short_name) %>%
  mutate(prior = crop_area_prior/sum(crop_area_prior)) %>%
  dplyr::select(-crop_area_prior, -adm) %>%
  rename(i = gridID, j = short_name, rev = prior)


# Check if total area adds to sum as in ag_stat
#sum(prior$crop_area_prior)
#sum(ag_stat_2000$value)

# save
# NB: data is only read correctly by GAMS if GRID_ID numbers are manually formated to number, zero digits
# Perhaps save as txt?
write_csv(prior, file.path(dataPath, "Model/Priors/priors.csv"), col_names = F)

