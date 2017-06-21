#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to downscale population projections
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


### LOAD grid MAP
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
plot(grid)


### LOAD GAUL MAPS
adm2_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000.rds"))
plot(adm2_map)


### URBAN MASK
urban_mask <- readRDS(file.path(dataPath, "Data/MWI/Processed/Spatial_data/urban_mask_MWI.rds"))


### COUNTRY SPECIFIC POPULATION MAP
# Load total population maps from WorldPop
tot_pop_af_2000 <- raster(file.path(dataPath, "Data/Global/WorldPop/Africa-POP-1KM_AFR_PPP_2000_adj_v2/AFR_PPP_2000_adj_v2.tif"))

# Crop country map
tot_pop_country_2000 <- crop(tot_pop_af_2000, adm2_map)
tot_pop_country_2000 <- mask(tot_pop_country_2000, adm2_map)

levelplot(tot_pop_country_2000, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm2_map, col = "black"))+
  layer(sp.polygons(urban_mask, col = "red"))
#plotKML(tot_pop_country_2000)

#click(tot_pop_country_2000)
#zoom(tot_pop_country_2000)

# Compare total pop with UN
# UN POP 2000 = 11,193
# Values are nearly the same, mismatch might be to bias created by overlay of polygon and raster
pop_check <- as.data.frame(rasterToPoints(tot_pop_country_2000))
sum(pop_check$AFR_PPP_2000_adj_v2)
rm(pop_check)


### CALCULATE TOTAL POPULATION PER ADM
# Get adm names
adm2_map_df <- adm2_map@data %>%
  transmute(ID = seq(1:length(.$ADM2_NAME)), adm2_GAUL = toupper(ADM2_NAME))

# Read adm mapping
MWI2adm2 <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm2, adm2_GAUL) %>%
  na.omit %>%
  unique()  

# Calculate totals
tot_pop_adm_2000 <- raster::extract(tot_pop_country_2000, adm2_map, df=T) %>%
  setNames(c("ID", "value")) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(value = sum(value, na.rm=T)) %>%
  left_join(., adm2_map_df) %>%
  left_join(., MWI2adm2) %>%
  dplyr::select(-ID, adm2_GAUL) %>%
  mutate(year = 2000) %>%
  filter(adm2 != "AREA_UNDER_NATIONAL_ADMINISTRATION")

# Save data
write_csv(tot_pop_adm_2000, file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_adm_2000_MWI.csv"))


### CALCULATE RURAL POPULATION PER GRID CELL
# Remove urban population using urban mask
tot_pop_rural_2000 <- mask(tot_pop_country_2000, urban_mask, inverse = T)

levelplot(tot_pop_rural_2000, par.settings = BTCTheme, margin = F) +
  layer(sp.polygons(adm2_map, col = "black"))+
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
write_csv(tot_pop_grid_2000, file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_grid_2000_MWI.csv"))
