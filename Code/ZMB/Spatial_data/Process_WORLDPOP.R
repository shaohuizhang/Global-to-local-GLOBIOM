#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to downscale population projections
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

#########################################################################
###           NB MAKE SURE TO SELECT THE APPROPRIATE ADM MAPS         ###
### ADM1 OR ADM2 DEPENDING ON AVAILABILITY OF AGRICULTURAL STATISTICS ###
#########################################################################


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


### LOAD grid MAP
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_", iso3c_sel, ".rds")))
plot(grid)


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))
plot(adm1)


### URBAN MASK
urban_mask <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/urban_mask_", iso3c_sel, ".rds")))
plot(urban_mask, col = "red")
plot(adm1, add = T)


### COUNTRY SPECIFIC POPULATION MAP
# Load total population maps from WorldPop
tot_pop_af_2000 <- raster(file.path(dataPath, "Data/Global/WorldPop/Africa-POP-1KM_AFR_PPP_2000_adj_v2/AFR_PPP_2000_adj_v2.tif"))

# Crop country map
tot_pop_country_2000 <- crop(tot_pop_af_2000, adm1)
tot_pop_country_2000 <- mask(tot_pop_country_2000, adm1)

levelplot(tot_pop_country_2000, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm1, col = "black"))+
  layer(sp.polygons(urban_mask, col = "red"))
#plotKML(tot_pop_country_2000)

#click(tot_pop_country_2000)
#zoom(tot_pop_country_2000)

# Compare total pop with UN
# UN POP 2000 = 10,531
# Values are nearly the same, mismatch might be to bias created by overlay of polygon and raster
pop_check <- as.data.frame(rasterToPoints(tot_pop_country_2000))
sum(pop_check$AFR_PPP_2000_adj_v2)
rm(pop_check)


### CALCULATE TOTAL POPULATION PER ADM
# Get adm names
adm1_df <- adm1@data %>%
  transmute(ID = seq(1:length(.$ADM1_NAME)), adm1_GAUL = toupper(ADM1_NAME))

# Read adm mapping
iso2adm1 <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_", iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()  

# Calculate totals
tot_pop_adm_2000 <- raster::extract(tot_pop_country_2000, adm1, df=T) %>%
  setNames(c("ID", "value")) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(value = sum(value, na.rm=T)) %>%
  left_join(., adm1_df) %>%
  left_join(., iso2adm1) %>%
  dplyr::select(-ID, adm1_GAUL) %>%
  mutate(year = 2000)

# Save data
write_csv(tot_pop_adm_2000, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/tot_pop_adm_2000_", iso3c_sel, ".csv")))


### CALCULATE RURAL POPULATION PER GRID CELL
# Remove urban population using urban mask
tot_pop_rural_2000 <- mask(tot_pop_country_2000, urban_mask, inverse = T)

levelplot(tot_pop_rural_2000, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm1, col = "black"))+
  layer(sp.polygons(urban_mask, col = "red"))


# Get grid names
grid_df <- grid@data %>%
  transmute(ID = seq(1:length(.$gridID)), gridID)

# Calculate totals
tot_pop_grid_2000 <- raster::extract(tot_pop_rural_2000, grid, df=T) %>%
  setNames(c("ID", "value")) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(value = sum(value, na.rm=T)) %>%
  left_join(., grid_df) %>%
  dplyr::select(-ID) %>%
  mutate(year = 2000)

# Save data
write_csv(tot_pop_grid_2000, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/tot_pop_grid_2000_", iso3c_sel, ".csv")))
