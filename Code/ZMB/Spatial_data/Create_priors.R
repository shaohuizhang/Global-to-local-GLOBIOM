#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to create priors
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "plotKML")


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


### LOAD DATA
# grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"

# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000.rds")))
plot(adm1)

# Urban mask
urban_mask <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/urban_mask_", iso3c_sel, ".rds")))
plot(urban_mask, col = "red")
plot(adm1, add = T)

# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Lc  
lc <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# Population map
pop <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/pop/pop_30sec_", iso3c_sel, ".tif")))
names(pop) <- "pop"

# Travel time map
tt <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/travel_time/travel_time_", iso3c_sel, ".tif")))
names(tt) <- "travel_time"


### CALCULATE RURAL POPULATION PER GRID CELL
pop_rural <- mask(pop, urban_mask, inverse = T)

levelplot(pop_rural, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm1, col = "black"))+
  layer(sp.polygons(urban_mask, col = "red"))


### PROCESS SUITABILITY DATA


### CREATE PRIOR DATABASE
# Create gridID and system combinations
priors_base <- expand.grid(gridID = unique(lc$gridID), sy = unique(lu_sy$sy))

# Stack maps with prior information and create database
prior_stack <- stack(pop_rural, tt, grid_30sec, gaez_stack)
plot(prior_stack)
prior_df <- as.data.frame(rasterToPoints(prior_stack)) %>% 
  filter(gridID %in% lc$gridID)


### FIX NA VALUES
# It appears that for some of the priors data is missing.
summary(prior_df)

# POP
# Cells in the country for which, probably no pop data was available
check_pop <- filter(prior_df, is.na(pop))
ggplot() +
  geom_polygon(data = adm1, aes(x = long, y = lat, group = group), fill = "white") +
  geom_raster(data = check_pop, aes(x = x, y = y), fill = "red") +
  coord_equal()

# GAEZ
# Cells at the border which are set to NA because of 30min grid
check_gaez <- filter(prior_df, is.na(maiz_S))
ggplot() +
  geom_polygon(data = adm1, aes(x = long, y = lat, group = group), fill = "white") +
  geom_raster(data = check_gaez, aes(x = x, y = y), fill = "red") +
  coord_equal()

# Fix by setting NA to 0
prior_dfx <- prior_df %>%
  gather(variable, value, -x, -y, -gridID) %>%
  mutate(value = ifelse(is.na(value), 0, value),
         value = value )


### CREATE GAMS PRIORS
# Subsistence system: S
# Priors defined by suitability index and rural population density




# Merge pop and crop cover data
priors <- priors_base %>%
  left_join(., prior_df)
  dplyr::select(gridID, system, prior)


popx <- as.data.frame(rasterToPoints(pop)) %>%
  filter
plot(pop)

# 
# # Calculate crop area prior for crops at adm2 level
# crop_area_prior_adm2 <- grid_sel %>%
#   group_by(adm2) %>%
#   mutate(pop_share = value/sum(value)) %>%
#   dplyr::select(gridID, adm = adm2, pop_share) %>%
#   left_join(.,lu_adm) %>%
#   mutate(crop_area_prior = pop_share * value) %>%
#   dplyr::select(gridID, adm, short_name, crop_area_prior)
# 
# # Calculate crop area prior for crops at adm0 level
# # Need to filter out crops for which adm2 data is available
# crops_adm2 <- unique(crop_area_prior_adm2$short_name)
# lu_adm0 <- filter(lu_adm, !(short_name %in% crops_adm2))
# 
# crop_area_prior_adm0 <- grid_sel %>%
#   group_by(adm0) %>%
#   mutate(pop_share = value/sum(value)) %>%
#   dplyr::select(gridID, adm = adm0, pop_share) %>%
#   left_join(., lu_adm0) %>%
#   mutate(crop_area_prior = pop_share * value) %>%
#   dplyr::select(gridID, adm, short_name, crop_area_prior)
# 
# # Calculate prior as share of total
# # Apply crop_cover
# prior <- bind_rows(crop_area_prior_adm0, crop_area_prior_adm2) %>%
#   ungroup() %>%
#   group_by(short_name) %>%
#   mutate(prior = crop_area_prior/sum(crop_area_prior)) %>%
#   dplyr::select(-crop_area_prior, -adm) %>%
#   rename(i = gridID, j = short_name, rev = prior)


# Check if total area adds to sum as in ag_stat
#sum(prior$crop_area_prior)
#sum(ag_stat_2000$value)



# Save data
saveRDS(prior_df, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/priors_", iso3c_sel, ".rds")))
