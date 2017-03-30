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
p_load("WDI", "countrycode", "plotKML")

### SET WORKING DIRECTORY
wdPath <- "~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### SOURCE
source("Code/process_large_raster_f.R")

### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()

### LOAD SIMU MAPS
SIMU_LU <- read_csv(file.path(dataPath, "GLOBIOM/simu_lu/SimUIDLUID.csv"))
ogrListLayers(file.path(dataPath, "GLOBIOM/simu_poly/SimU_all.shp"))
SIMU_5min_poly <- readOGR(file.path(dataPath, "GLOBIOM/simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 894 = Malawi)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==894,]

### LOAD GADM MAPS
# Load previously saved map (Download_GADM.r)
country_map_raw <- readRDS(file.path(dataPath, "Processed\\ZMB\\GADM_maps/GADM_2.8_ZMB_adm1.rds"))
spplot(country_map_raw, "OBJECTID")

### LOAD LAND COVER MAP 
# Load map
#land_cover_map_raw <- raster(file.path(dataPath, "Raw\\ZMB\\Land_cover_maps\\Zambia_LandCover_2010_Scheme_II\\Zambia_Landcover2_2010_Scheme_II.tif"))
land_cover_map_raw <- raster(file.path(" H:\\TEMP\\Zambia_LandCover_2010_Scheme_II\\zambia_landcover_2010_scheme_ii.tif"))
land_cover_map_raw

levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme)
levels(land_cover_map_raw)

### REPROJECT MAPS
#' It is essential to use the same projection for all maps. If there is a difference we reproject the SIMU and country_map
#' Another route would be to reproject the country land cover map to the global SIMU projection. 
#' This is however not recomendable as land cover maps tend to have a very high resolution. Reprojection will therefore take 
#' a lot of time and potentially introduces distortions. 
#' When reprojecting the SIMU map it is important to set the method to "ngb" as the map presents categorical values, otherwise SIMU IDs will be somehow averaged.
#' It is important to save files to file using filename = or remove temporary files otherwise the HD might be full quickly!

# Compare CRS of SIMU and target country land cover map
# Same projections so no reprojection needed.
crs(land_cover_map_raw)
crs(country_map_raw)
crs(SIMU2country_poly)

# Set country crs
#country_crs <- crs(land_cover_map_raw)

# Reproject SIMU_poly to country CRS
#SIMU2country_poly_rp <- spTransform(SIMU2country_poly, country_crs)
levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(SIMU2country_poly, col = "black"))

# Reproject country_map to country CRS
#country_map_rp <-  spTransform(country_map_raw, country_crs)
levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(country_map_raw, col = "black"))

### LINK SIMU WITH LAND COVER DATA
# Create SimuID list
# Add unique as several SimuS consist of multiple polygons!
SimUID_list <- unique(SIMU2country_poly@data$SimUID)

# Overlay land cover map and SIMU polygon per simu
# function to extract values by SimU (polygon)
extract_simu_f <- function(polyID){
  print(polyID)
  poly_simu <- SIMU2country_poly[SIMU2country_poly$SimUID==polyID,]
  df_poly <- raster::extract(land_cover_map_raw, poly_simu, df = T) %>%
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
land_cover_shares_raw <- bind_rows(lapply(SimUID_list, extract_simu_f))
saveRDS(land_cover_shares_raw, file.path(dataPath, "Processed\\ZMB\\Spatial_data/land_cover_shares_2010_ZMB_raw.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Processed\\ZMB\\Spatial_data/land_cover_shares_2010_ZMB_raw.rds"))

# Check total of shares
check_total <- land_cover_shares_raw %>%
  gather(variable, value, -SimUID) %>%
  group_by(SimUID) %>%
  summarize(total = sum(value, na.rm=T))

land_cover_shares <- land_cover_shares_raw %>%
  dplyr::select(SimUID, everything()) %>%
  replace(is.na(.), 0) 

  
# VALIDATE RESULTS
# Overlap seems ok
# Compare results with land cover map
waterbody <- land_cover_shares$SimUID[land_cover_shares$"11" > 0.1]
settlements <- land_cover_shares$SimUID[land_cover_shares$"12" > 0.005]
cropland <- land_cover_shares$SimUID[land_cover_shares$"9" > 0.7]
missing <- land_cover_shares$SimUID[land_cover_shares$"0" > 0]

# Create mask for specific class: 9 annual cropland
check_r <- keep_value_r_f(land_cover_map_raw, 9, filename= "Cache/check.grd")
plot(check_r)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% cropland,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/checkr.gri")

# Create mask for specific class: 11 waterbody
check_r <- keep_value_r_f(land_cover_map_raw, 11, filename= "Cache/check.grd")
plot(check_r)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% waterbody,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/checkr.gri")

# Create mask for specific class: 12 settlements
check_r <- keep_value_r_f(land_cover_map_raw, 12, filename= "Cache/check.grd")
plot(check_r)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% settlements,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/checkr.gri")

# plot SIMUs with missing data
# Appears to be on the Northern border
plot(land_cover_map_raw)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% missing,], add = T, border = "black")

# Overlay maps on Google Earth: Install Google Earth First 
# Settlements overlap but for some reason Lusaka is not identified as settlement
check <- SIMU2country_poly[SIMU2country_poly$SimUID %in% settlements,]
plot(check)
plotKML(check)

# CREATE FINAL SIMU FILE AND WRITE TO GDX
