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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD SIMU MAPS
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"))
simu_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 454 = Malawi)
simu2country_poly <- simu_5min_poly[simu_5min_poly$COUNTRY==454,]

### LOAD GAUL MAPS
country_map <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\Maps/GAUL_MWI_adm2_2000.rds"))


### CALCULATE SHARE SIMU IN ADM
# We use package sf and need to convert all polygons to sf class
# We only select relevant polygon information
country_map_sf <- st_as_sf(country_map) %>%
  dplyr::select(ADM2_NAME, geometry)
simu2country_poly_sf <- st_as_sf(simu2country_poly) %>%
  dplyr::select(SimUID, geometry)

plot(country_map)
plot(simu2country_poly_sf, add = T)

# Determine intersection of simu polygons with adm2 to calculate the share of simu that lies within an adm.
# This is to check if simus are fragmented over multiple adms.

simu_adm <- st_intersection(country_map_raw_sf, simu2country_poly_sf)  
pi2 <- st_intersection(simu2country_poly_sf, country_map_raw_sf)  

# add in areas in m2
area_overlap <- pi2 %>% 
  mutate(area = st_area(.) %>% as.numeric())



# for each field, get area per soil type
area2 <- area_overlap %>% 
  as_tibble() %>% 
  group_by(SimUID, ADM2_NAME) %>% 
  summarize(area = sum(area)) %>%
  ungroup() %>%
  group_by(SimUID) %>%
  mutate(area_adm = sum(area)) %>%
  mutate(share = area/area_adm) %>%
  mutate(check = sum(share))

all.equal(area, area2)
test <- attArea %>% 
  as_tibble() %>% 
  group_by(ADM2_NAME) %>% 
  summarize(area = sum(area)) %>%
  left_join(area_adm)


plot(country_map_test, lwd = 2)
plot(poly_test2[poly_test2$SimUID == 147914,], add = T, col ='red')