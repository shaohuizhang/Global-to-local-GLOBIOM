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


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


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
# Load map
land_cover_map_raw <- raster(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 2\\malawi 1990 classification scheme2.img"))
land_cover_map_raw

# Save land class
land_class <- levels(land_cover_map_raw)[[1]]
write_csv(land_class, file.path(dataPath, "Data/MWI/processed/Spatial_data/land_class_MWI.csv"))


### REPROJECT MAPS
#' It is essential to use the same projection for all maps. If there is a difference we reproject the SIMU and country_map
#' Another route would be to reproject the country land cover map to the global SIMU projection. 
#' This is however not recomendable as land cover maps tend to have a very high resolution. Reprojection will therefore take 
#' a lot of time and potentially introduces distortions. 
#' When reprojecting the SIMU map it is important to set the method to "ngb" as the map presents categorical values, otherwise SIMU IDs will be somehow averaged.
#' It is important to save files to file using filename = or remove temporary files otherwise the HD might be full quickly!

# Compare CRS of SIMU and target country land cover map
crs(land_cover_map_raw)
crs(country_map)

# Set country crs
country_crs <- crs(land_cover_map_raw)

# Reproject SIMU_poly to country CRS
simu2country_poly <- spTransform(simu2country_poly, country_crs)

# Reproject country_map to country CRS
country_map <-  spTransform(country_map, country_crs)

# Add colours based on RGB information
map_colour <- levels(land_cover_map_raw)[[1]] %>%
  mutate(colour = rgb(Red, Green, Blue, max = 255),
         colour = ifelse(colour == "#000000", "#FFFFFF", colour)) %>% # replace black with white for shadows and no data
  unique()
map_colour <- map_colour[order(map_colour$Land_Cover, decreasing = F),]

# Plot
levelplot(land_cover_map_raw, att='Land_Cover', col.regions = map_colour$colour, margin = F) +
  layer(sp.polygons(simu2country_poly, col = "black", lwd = 2))

levelplot(land_cover_map_raw, att='Land_Cover', col.regions = map_colour$colour, margin = F) +
  layer(sp.polygons(country_map, col = "black", lwd = 2))


### LINK SIMU WITH LAND COVER DATA
# Create SimuID list
# Add unique as several SimuS consist of multiple polygons!
simuid_list <- unique(simu2country_poly@data$SimUID)

# Overlay land cover map and SIMU polygon per simu
# function to extract values by SimU (polygon)
extract_simu_f <- function(polyID){
  print(polyID)
  poly_simu <- simu2country_poly[simu2country_poly$SimUID==polyID,]
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
land_cover_shares_raw <- bind_rows(lapply(simuid_list, extract_simu_f))
saveRDS(land_cover_shares_raw, file.path(dataPath, "Data\\MWI\\Processed\\Spatial_data/land_cover_shares_2000_MWI_raw.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\Spatial_data/land_cover_shares_2000_MWI_raw.rds"))

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
waterbody <- land_cover_shares$SimUID[land_cover_shares$"11" > 0.5]
settlements <- land_cover_shares$SimUID[land_cover_shares$"12" > 0.1]
annual_cropland <- land_cover_shares$SimUID[land_cover_shares$"9" > 0.7]

# Create mask for specific class: 9 annual cropland
mask3 <- setValues(raster(land_cover_map_raw), NA)
mask3[land_cover_map_raw==9] <- 9
plot(mask3)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% annual_cropland,], add = T, border = "red")
rm(mask3)

# Create mask for specific class: 11 waterbody
mask4 <- setValues(raster(land_cover_map_raw), NA)
mask4[land_cover_map_raw==11] <- 11
plot(mask4)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% waterbody,], add = T, border = "red")
rm(mask4)

# Create mask for specific class: 12 settlements
mask5 <- setValues(raster(land_cover_map_raw), NA)
mask5[land_cover_map_raw==12] <- 12
plot(mask5)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% settlements,], add = T, border = "red")
rm(mask5)

# plot one SIMU with lot of missing data
# Appears to be on the Northern border
plot(land_cover_map_raw)
plot(SIMU2country_poly[SIMU2country_poly$SimUID %in% 177235,], add = T, border = "red")

# Overlay maps on Google Earth: Install Google Earth First 
# Settlements perfectly overlap!
check <- SIMU2country_poly[SIMU2country_poly$SimUID %in% settlements,]
plot(check)
plotKML(check)

# SAVE LAND COVER FILE
saveRDS(land_cover_shares,  file.path(dataPath, "Data\\MWI\\Processed\\Spatial_data\\land_cover_shares_2000_MWI.rds"))
