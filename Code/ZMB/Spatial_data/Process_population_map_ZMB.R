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
p_load("WDI", "countrycode", "R.utils", "gdxrrw")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"
SSPPath <- "C:\\Users\\vandijkm\\DATA"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### GET SIMU MAPS
#SIMU_LU <- read_csv(file.path(dataPath, "GLOBIOM/Simu/simu_lu/SimUIDLUID.csv"))
SIMU_5min <- raster(file.path(dataPath, "GLOBIOM/Simu/simu_raster_5min/rasti_simu_gr.tif"))

### COUNTRY SPECIFIC POPULATION MAP
# Load total population maps from WorldPop
tot_pop_af_2000 <- raster(file.path(SSPPath, "WorldPop/Africa-POP-1KM_AFR_PPP_2000_adj_v2/AFR_PPP_2000_adj_v2.tif"))

# Load GADM
country_map <- readRDS(file.path(dataPath, "Data\\Processed\\ZMB\\GADM_maps/GADM_2.8_ZMB_adm2.rds"))
spplot(country_map, "OBJECTID")

# Crop country map
tot_pop_country_2000 <- crop(tot_pop_af_2000, country_map)
tot_pop_country_2000 <- mask(tot_pop_country_2000, country_map)

levelplot(tot_pop_country_2000, par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(country_map, col = "black"))

# Compare total pop with UN
# UN POP 2000 = 10,585
# Values are nearly the same, mismatch might be to bias created by overlay of polygon and raster
pop_check <- as.data.frame(rasterToPoints(tot_pop_country_2000))
sum(pop_check$AFR_PPP_2000_adj_v2)
rm(pop_check)

### CALCULATE TOTAL POPULATION PER ADM
# Get adm names
country_map_df <- country_map@data %>%
  dplyr::select(ID = OBJECTID, adm1 = NAME_1, adm2 = NAME_2)
  
# Calculate totals
tot_pop_adm_2000 <- raster::extract(tot_pop_country_2000, country_map, df=T) %>%
  setNames(c("ID", "value")) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(value = sum(value, na.rm=T)) %>%
  left_join(., country_map_df) %>%
  dplyr::select(-ID) %>%
  mutate(year = 2000)

# Save data
write_csv(tot_pop_adm_2000, file.path(dataPath, "Data/Processed/ZMB/Spatial_data/tot_pop_adm_2000_ZMB.csv"))
