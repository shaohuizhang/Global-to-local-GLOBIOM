#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to calculate share of simu in adm
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
p_load("WDI", "countrycode", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD SIMU MAP
simu2country_poly <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/simu_MWI.rds"))


### LOAD GAUL ADM MAP
country_map <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\Maps/GAUL_MWI_adm2_2000.rds"))


### CALCULATE SHARE SIMU IN ADM
# We use package sf and need to convert all polygons to sf class
# Note that the relevant adm level name needs to be selected
country_map_sf <- st_as_sf(country_map) %>%
  dplyr::select(ADM2_NAME, geometry)
simu2country_poly_sf <- st_as_sf(simu2country_poly) %>%
  dplyr::select(SimUID, geometry)

plot(country_map)
plot(simu2country_poly_sf, add = T)

# Determine intersection of simu polygons with adm2 to calculate the share of simu that lies within an adm.
# This is to check if simus are fragmented over multiple adms.
simu_adm <- st_intersection(country_map_sf, simu2country_poly_sf)  

# Add in areas in m2
area_simu_adm <- simu_adm %>% 
  mutate(area = st_area(.) %>% as.numeric())

# For each simu, get area per adm
area_sh_simu_in_adm <- area_simu_adm %>% 
  as_tibble() %>% 
  group_by(SimUID, ADM2_NAME) %>% 
  summarize(area = sum(area)) %>%
  ungroup() %>%
  group_by(SimUID) %>%
  mutate(area_simu = sum(area)) %>%
  mutate(share = area/area_simu) %>%
  dplyr::select(SimUID, ADM2_NAME, share) %>%
  spread(ADM2_NAME, share) %>%
  replace(is.na(.), 0) %>% # Add zeros where there are no values
  gather(ADM2_NAME, share, -SimUID) %>%
  rename(adm2_GAUL = ADM2_NAME) %>% # set standard name for adm
  mutate(adm2_GAUL = toupper(adm2_GAUL))

# Save
write_csv(area_sh_simu_in_adm, file.path(dataPath, "Data/MWI/Processed/Spatial_data/area_sh_simu_in_adm_MWI.csv"))
