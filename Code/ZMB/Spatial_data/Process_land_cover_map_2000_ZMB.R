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
source("Code/R2GDX.R")

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
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\ZMB\\Spatial_data\\Zambia_LandCover_2000_Scheme_II\\Zambia_Landcover2_2000_Scheme_II.tif"))
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

levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(SIMU2country_poly, col = "black"))

levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(country_map_raw, col = "black"))

### LINK SIMU WITH LAND COVER DATA
# SIMU link to ID
p_df <- data.frame(ID=1:length(SIMU2country_poly), SIMU =  SIMU2country_poly@data$SimUID)

# Overlay land cover map and SIMU polygon (THIS TAKES SOME TIME)
land_cover_shares_raw <- raster::extract(land_cover_map_raw, SIMU2country_poly, df=T) 
saveRDS(land_cover_shares_raw, file.path(dataPath, "Processed\\ZMB\\Spatial_data/land_cover_shares_2000_ZMB_raw.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Processed\\ZMB\\Spatial_data/land_cover_shares_2000_ZMB_raw.rds"))

# Calculate land cover shares per SIMU
land_cover_shares <- land_cover_shares_raw %>%
  rename(class = malawi_2000_classification_scheme2) %>%
  na.omit() %>%
  group_by(ID, class) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n, na.rm = T)) %>%
  dplyr::select(-n) %>%
  spread(class, freq) %>%
  left_join(.,p_df)

# VALIDATE RESULTS
# Overlap seems ok
# Compare results with land cover map
waterbody <- land_cover_shares$SIMU[land_cover_shares$"11" > 0.5]
settlements <- land_cover_shares$SIMU[land_cover_shares$"12" > 0.1]
Annual_cropland <- land_cover_shares$SIMU[land_cover_shares$"9" > 0.7]

# Create mask for specific class: 9 annual cropland
mask3 <- setValues(raster(land_cover_map_raw), NA)
mask3[land_cover_map_raw==9] <- 9
plot(mask3)
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% cropland,], add = T, border = "red")
rm(mask3)

# Create mask for specific class: 11 waterbody
mask4 <- setValues(raster(land_cover_map_raw), NA)
mask4[land_cover_map_raw==11] <- 11
plot(mask4)
plot(SIMU2country_poly_rp, add = T, border = "black")
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% waterbody,], add = T, border = "red")
rm(mask4)

# Create mask for specific class: 12 settlements
mask5 <- setValues(raster(land_cover_map_raw), NA)
mask5[land_cover_map_raw==12] <- 12
plot(mask5)
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% settlements,], add = T, border = "red")
rm(mask5)

# plot one SIMU with lot of missing data
# Appears to be on the Northern border
plot(land_cover_map_raw)
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% 177235,], add = T, border = "red")

# Check individual SIMUs
SIMU2country_id <- SIMU2country_poly
SIMU2country_id$name <- SIMU2country_id$SimUID
identify(map(SIMU2country_id))

# Overlay maps on Google Earth: Install Google Earth First 
# Settlements perfectly overlap!
check <- SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% settlements,]
plot(check)
plotKML(check)

# CREATE FINAL SIMU FILE AND WRITE TO GDX
land_cover_SIMU <- land_cover_shares %>%

  mutate(LC = recode(class, c(`1` = "Forest", `2` = "Grass", `3` = "CrpLnd", 
                              `4` = "WetLnd", `5` = "?", `6` = "OthNatLnd", 
                              `7` = "NotRel", `8` = "NotRel"))  
  
  
test <- rastertoPoints(land_cover_map_raw)