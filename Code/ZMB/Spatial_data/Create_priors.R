#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to create priors
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# NB BETTER TO INCORPORATE FIXED SHARES FROM IRR MAP AS PRIOR!
# Meaning priors should be overwritten by shares for which we have information. 

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
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))
plot(adm)

# Urban mask
urban_mask <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/urban_mask/urban_mask_", iso3c_sel, ".rds")))
plot(urban_mask, col = "red")
plot(adm, add = T)

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

# GAEZ 
gaez_files <- list.files(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaez/30sec")), full.names = T)
gaez <- stack(gaez_files)

# remove country and grid info in names
names(gaez) <- gsub(paste0("_30sec_", iso3c_sel), "", names(gaez))
sort(unique(lu_sy$sy))


### CALCULATE RURAL POPULATION PER GRID CELL
pop_rural <- mask(pop, urban_mask, inverse = T)

levelplot(pop_rural, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm, col = "black"))+
  layer(sp.polygons(urban_mask, col = "red"))


### CREATE PRIOR DATABASE
# Create gridID and system combinations
priors_base <- expand.grid(gridID = unique(lc$gridID), sy = unique(lu_sy$sy))

# Check gaez
check_gaez <- as.data.frame(rasterToPoints(gaez))
summary(check_gaez)

# Stack maps with prior information and create database
prior_stack <- stack(pop_rural, grid_30sec, gaez)
prior_df <- as.data.frame(rasterToPoints(prior_stack)) %>% 
  filter(gridID %in% lc$gridID) %>%
  gather(sy, suit, -gridID, -pop, -x, -y)
  #dplyr::select(-x, -y) %>%


### FIX NA VALUES
# It appears that for some of the priors data is missing.
summary(prior_df)
check <- filter(prior_df, is.na(suit)) 
unique(check$gridID)

# POP
# Cells in the country for which, probably no pop data was available
check_pop <- filter(prior_df, is.na(pop))
ggplot() +
  geom_polygon(data = adm, aes(x = long, y = lat, group = group), fill = "white") +
  geom_raster(data = check_pop, aes(x = x, y = y), fill = "red") +
  coord_equal()

# GAEZ
# Cells at the border which are set to NA because of 30min grid
check_gaez <- filter(prior_df, is.na(suit))
ggplot() +
  geom_polygon(data = adm, aes(x = long, y = lat, group = group), fill = "white") +
  geom_raster(data = check_gaez, aes(x = x, y = y), fill = "red") +
  coord_equal()

# Fix by setting NA to 0
prior_df <- prior_df %>%
  mutate(suit = ifelse(is.na(suit), 0, suit),
         pop = ifelse(is.na(pop), 0, pop))
  
# Normalize pop using minimax approach
# Divide suitability by 10000
# Calculate priors by geometric average of pop and suit
# NOTE that for some crops suit = 0 all the way.CHECK
prior_df <- prior_df %>%
  group_by(sy) %>%
  mutate(pop_norm = (pop- min(pop))/(max(pop)-min(pop))) %>%
  ungroup() %>%
  mutate(suit = suit/10000,
         priors = sqrt(pop_norm*suit)) %>%
  group_by(sy) %>%
  mutate(priors_norm = priors/sum(priors),
         priors_norm = ifelse(is.nan(priors_norm), 0, priors_norm)) %>%
  ungroup() %>%
  dplyr::select(gridID, sy, priors_norm)
  
sum(prior_df$priors_norm)
summary(prior_df)

# Save data
saveRDS(prior_df, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/priors_2000_", iso3c_sel, ".rds")))
