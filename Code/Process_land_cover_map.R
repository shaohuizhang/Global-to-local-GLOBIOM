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
wdPath<-"~/Global-to-local-GLOBIOM"
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

# Obtain country poly (using iso3c numbering: 454 = Malawi)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==454,]

### LOAD GADM MAPS
# Load previously saved map (Download_GADM.r)
country_map_raw <- readRDS("Data/GADM_2.8_MWI_adm1.rds")
spplot(country_map_raw, "OBJECTID")

### LOAD LAND COVER MAP 
# Load map
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\MWI\\Land_cover_maps\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 1\\malawi 2000 classification scheme1.img"))
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
crs(land_cover_map_raw)
crs(country_map_raw)
crs(SIMU_5min)

# Set standard crs
standard_crs <- crs(SIMU_5min)
country_crs <- crs(land_cover_map_raw)

# Reproject SIMU_poly to country CRS
SIMU2country_poly_rp <- spTransform(SIMU2country_poly, country_crs)
levelplot(land_cover_map_raw, att='ID', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(SIMU2country_poly_rp, col = "black"))

# Reproject country_map to country CRS
country_map_rp <-  spTransform(country_map_raw, country_crs)
levelplot(land_cover_map_raw, att='ID', par.settings = RdBuTheme, margin = F) +
  layer(sp.polygons(country_map_rp, col = "black"))

### LINK SIMU WITH LAND COVER DATA
# SIMU link to ID
p_df <- data.frame(ID=1:length(SIMU2country_poly_rp), SIMU =  SIMU2country_poly_rp@data$SimUID)

# Overlay land cover map and SIMU polygon (THIS TAKES SOME TIME)
#land_cover_shares_raw <- raster::extract(land_cover_map_raw, SIMU2country_poly_rp, df=T) 
#saveRDS(land_cover_shares_raw, file.path(dataPath, "Processed\\MWI\\Land_cover_maps/land_cover_shares_2000_raw.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Processed\\MWI\\Land_cover_maps/land_cover_shares_2000_raw.rds"))

# Calculate land cover shares per SIMU
land_cover_shares <- land_cover_shares_raw %>%
  rename(class = malawi_2000_classification_scheme1) %>%
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
wetlands <- land_cover_shares$SIMU[land_cover_shares$"4" > 0.5]
missing <- land_cover_shares$SIMU[land_cover_shares$"1" > 0.9]
settlements <- land_cover_shares$SIMU[land_cover_shares$"5" > 0.1]
cropland <- land_cover_shares$SIMU[land_cover_shares$"3" > 0.7]

# Create mask for specific class: 3 cropland
mask3 <- setValues(raster(land_cover_map_raw), NA)
mask3[land_cover_map_raw==3] <- 3
plot(mask3)
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% cropland,], add = T, border = "red")

# Create mask for specific class: 4 wetlands
mask4 <- setValues(raster(land_cover_map_raw), NA)
mask4[land_cover_map_raw==4] <- 4
plot(mask4)
plot(SIMU2country_poly_rp, add = T, border = "black")
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% wetlands,], add = T, border = "red")

# Create mask for specific class: 5 settlements
mask5 <- setValues(raster(land_cover_map_raw), NA)
mask5[land_cover_map_raw==5] <- 5
plot(mask5)
plot(SIMU2country_poly_rp[SIMU2country_poly_rp$SimUID %in% settlements,], add = T, border = "red")

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
  
  
