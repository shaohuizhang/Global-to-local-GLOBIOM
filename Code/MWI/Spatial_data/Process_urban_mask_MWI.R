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
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "plotKML", "ggthemes")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD GAUL MAPS
adm2_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000.rds"))
plot(adm2_map)


### URBAN MASK
# Load global mask
urban_mask_raw <- readOGR(file.path(dataPath, "Data/Global/GRUMPv1/global_urban_extent_polygons_v1.01.shp"))

# Select country information
iso3c_sel <- "MWI"
urban_mask <- urban_mask_raw[urban_mask_raw$ISO3 == iso3c_sel,]                 

# City information
data(world.cities)
cities <- world.cities %>%
  mutate(iso3c = countrycode(country.etc, "country.name", "iso3c")) %>%
  filter(iso3c == iso3c_sel)

# Plot
adm2_map_sf <- st_as_sf(adm2_map)
urban_mask_sf <- st_as_sf(urban_mask)

ggplot() + 
  geom_sf(data = adm2_map_sf) +
  geom_sf(data = urban_mask_sf, fill = "red") +
  geom_point(data = cities, aes(x = long, y = lat), colour = "green") 


# Save data
saveRDS(urban_mask, file.path(dataPath, "Data/MWI/Processed/Spatial_data/urban_mask_MWI.rds"))
