#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to process land cover map and aggregate to SIMU level
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
p_load("WDI", "countrycode", "plotKML", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SOURCE
source("Code/process_large_raster_f.R")


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD SIMU MAP
simu2country_poly <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/simu_MWI.rds"))
plot(simu2country_poly)


### LOAD GAUL MAPS
country_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000.rds"))
plot(country_map)


### LOAD LAND COVER MAP 
# Load global ESA map
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
land_cover_map_ESA


# REPROJECT, IF NEEDED
# Compare CRS of SIMU and target country land cover map
# Same projections so no reprojection needed.
crs(land_cover_map_ESA)
crs(country_map)
crs(simu2country_poly)

# Reproject, if needed

# Set country crs
#country_crs <- crs(land_cover_map)

# Reproject SIMU_poly to country CRS
#simu2country_poly <- spTransform(simu2country_poly, country_crs)

# Reproject country_map to country CRS
#country_map <-  spTransform(country_map, country_crs)


### PREPARE COUNTRY LAND COVER MAP
# Mask takes a long time and is not needed
land_cover_map <- crop(land_cover_map_ESA, country_map)
levelplot(land_cover_map, par.settings = RdBuTheme)

# Add attributes
# Load ESA legend
ESA_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv")) %>%
  mutate(ID = land_cover_code)


# Add attributes
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
land_cover_map <- ratify(land_cover_map)
rat <- levels(land_cover_map)[[1]] #get the values of the unique cell frot the attribute table
rat <- left_join(rat, ESA_legend)

# Create colours for legend and sort in right order
ESA_colour <- rat
ESA_colour <- ESA_colour %>%
  filter(ID %in% seq(0, 220, 10)) %>%
  mutate(colour= rgb(R, G, B, max = 255)) %>%
  unique()
ESA_colour <- ESA_colour[order(ESA_colour$land_cover_short, decreasing = F),]

# Links levels
levels(land_cover_map) <- rat
levels(land_cover_map)
rm(rat)

# Visualise 
levelplot(land_cover_map, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
  layer(sp.polygons(simu2country_poly, col = "black", lwd = 2)) +
  layer(sp.polygons(country_map, col = "red", lwd = 2))


### CALCULATE LAND COVER SHARES
# NB: shares are determined on relative numbers of raster cells, which might differ slighty over latitude. 
# An improvement could be to multiply the cells with their area and then calculate shares, which requires more thinking.
# However a comparison of cell size indicated that differences are very small, also because we are looking at a limited area.
# So we do not expect much differences.

# Create SimuID list
# Add unique as several SimuS consist of multiple polygons!
simuid_list <- unique(simu2country_poly@data$SimUID)

# Overlay land cover map and SIMU polygon per simu
# function to extract values by SimU (polygon)
extract_simu_f <- function(polyID){
  print(polyID)
  poly_simu <- simu2country_poly[simu2country_poly$SimUID==polyID,]
  df_poly <- raster::extract(land_cover_map, poly_simu, df = T) %>%
    setNames(c("ID", "class")) %>%
    na.omit() %>%
    group_by(class) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n, na.rm = T)) %>%
    dplyr::select(-n) %>%
    spread(class, freq) %>%
    mutate(SimUID = polyID)
  return(df_poly)
}

# Run function and combine
land_cover_shares_raw <- bind_rows(lapply(simuid_list, extract_simu_f))
saveRDS(land_cover_shares_raw, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_ESA_2000_MWI_raw.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_ESA_2000_MWI_raw.rds"))

# Check total of shares
check_total <- land_cover_shares_raw %>%
  gather(variable, value, -SimUID) %>%
  group_by(SimUID) %>%
  summarize(total = sum(value, na.rm=T))

land_cover_shares <- land_cover_shares_raw %>%
  dplyr::select(SimUID, everything()) %>%
  replace(is.na(.), 0) 

  
# VALIDATE RESULTS
# Compare results with land cover map
waterbody <- land_cover_shares$SimUID[land_cover_shares$"210" > 0.1]
urban_areas <- land_cover_shares$SimUID[land_cover_shares$"190" > 0.03]
cropland <- land_cover_shares$SimUID[land_cover_shares$"10" > 0.2]
missing <- land_cover_shares$SimUID[land_cover_shares$"0" > 0]

# Create mask for specific class: 10 annual cropland
check_r <- keep_value_r_f(land_cover_map, 10, filename= "Cache/check.grd")
plot(check_r, col = "black")
plot(country_map, add = T, border = "blue")
plot(simu2country_poly[simu2country_poly$SimUID %in% cropland,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/check.gri")

# Create mask for specific class: 210 waterbody
check_r <- keep_value_r_f(land_cover_map, 210, filename= "Cache/check.grd")
plot(check_r, col = "black")
plot(country_map, add = T, border = "blue")
plot(simu2country_poly[simu2country_poly$SimUID %in% waterbody,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/check.gri")

# Create mask for specific class: 190 urban areas
check_r <- keep_value_r_f(land_cover_map, 190, filename= "Cache/check.grd")
plot(check_r, col = "black")
plot(country_map, add = T, border = "blue")
plot(simu2country_poly[simu2country_poly$SimUID %in% urban_areas,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/checkr.gri")

# plot SIMUs with missing data
# No missing data

# Overlay maps on Google Earth: Install Google Earth First 
# Settlements overlap but for some reason Lusaka is not identified as settlement
check <- simu2country_poly[simu2country_poly$SimUID %in% urban_areas,]
plotKML(check)

### CALCULATE LAND COVER AREA PER SIMU
# Read simu area information
simu_area <- read_csv(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_info/simu_area.csv"))

# Multiply land cover shares with area and map to GLOBIOM lc classes.
land_cover_area <- land_cover_shares %>%
  gather(land_cover_code, share, - SimUID) %>%
  left_join(., simu_area) %>%
  mutate(area = share * simu_area) %>%
  dplyr::select(SimUID, land_cover_code, area) %>%
  filter(area > 0) 

# Save data
write_csv(land_cover_area, file.path(dataPath, "Data/MWI/Processed/Spatial_data/land_cover_area_ESA_2000_MWI.csv"))

### CALCULATE LAND COVER AREA PER ADM
# We use package sf and need to convert all polygons to sf class
# Note that the relevant adm level name needs to be selected

# Caluculate area in ha
adm_area <- st_as_sf(country_map) %>%
  mutate(area = as.numeric(st_area(.)*0.0001)) %>% 
  as.data.frame() %>%
  transmute(adm2_GAUL = toupper(ADM2_NAME), area)

# adm names
adm_names <- country_map@data %>%
  transmute(adm2_GAUL = toupper(ADM2_NAME)) %>%
  mutate(adm_id = seq(1:length(.$adm2_GAUL)))

# Calculate shares of land cover in adm
land_cover_adm <- raster::extract(land_cover_map, country_map, df = T) %>%
  setNames(c("adm_id", "land_cover_code")) %>%
  na.omit() %>%
  group_by(adm_id, land_cover_code) %>%
  summarize(n = n()) %>%
  mutate(share = n / sum(n, na.rm = T)) %>%
  dplyr::select(-n) %>%
  left_join(., adm_names) %>%
  left_join(., adm_area) %>%
  mutate(area = area*share) %>%
  ungroup() %>%
  dplyr::select(adm2_GAUL, land_cover_code, area) %>%
  left_join(ESA_legend) %>%
  dplyr::select(-R, -G, -B,-ID)

# Save data
write_csv(land_cover_adm, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_adm_2000_MWI.csv"))


