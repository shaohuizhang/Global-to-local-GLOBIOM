#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("XLConnect", "measurements", "nominatim")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD MAPS
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
GMIA <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GMIA_MWI.rds"))

# DEFINE PROJECTION AND REPROJECT
# Projection of irrigation data is UTM 36 South (Arc 1960) - printed on maps in report
# This is equal to EPSG:21036.
crs_ir <- "+init=EPSG:21036" 
crs <- CRS("+init=EPSG:4326") # WSG84

# Reproject adm and grid
# Projection is the same but have to be identically specified for some functions to work
adm2 <-  spTransform(adm2, crs)
grid <-  spTransform(grid, crs)
GMIA <-  projectRaster(GMIA, crs = crs)


### PROCESS AND PLOT ESTATE IRRIGATION DATA
# Load data
ir_est_raw <- read_excel(file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/Other/Irrigation/Estate_irrigation_raw.xlsx"))

# Add projection to points and reproject to WSG84
ir_est_geo <- ir_est_raw %>%
  filter(!is.na(easting) & !is.na(northing)) %>%
  mutate(lat = northing, lon = easting) %>%
  as.data.frame()
coordinates(ir_est_geo) <- c("lon", "lat")
proj4string(ir_est_geo) <- CRS(crs_ir)
ir_est_geo <- spTransform(ir_est_geo, crs)

# Create data frame with all info and add radius needed to create circle with same same as actual_ha
# A = pi x r2 => r = sqrt(A/pi)
ir_est_df <- as.data.frame((ir_est_geo)) %>%
  mutate(radius = sqrt(actual_ha*10000/pi),
         type = "Estate")

# Plot GMIA, adm2 and irr
plot(GMIA)
plot(adm2, add = T)
plot(ir_est_geo, add = T, col = "red")
axis(1)
axis(2)


pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(GMIA),
                    na.color = "transparent")

leaflet() %>% 
  addTiles() %>% 
  addCircles(data = ir_est_df, lat = ~ lat, lng = ~ lon,  label = ~as.character(paste(site, actual_ha, sep = "_")),
             radius = ~ radius, weight = 1, fillOpacity = 0.1) %>%
  addRasterImage(GMIA, colors = pal, opacity = 0.5) %>%
  addLegend(values = values(GMIA), pal = pal, 
            title = "irrigated area")


### IMPROVE LOCATION OF IRRIGATED AREAS BY UPDATING COORDINATES

# Tea estates featured on GMIA but not on irrigation plan
# Not added because we cannot find information on size, etc
#estate_miss <- c("Kawalazi", "Chombe tea estate")

# Estates not correctly geo_coded
estate_wrong <- c("Dwangwa estate", "Nchalo sucoma sugar estate", "Alimenda estate")

# Geocode missing/wrong estates
estate_geo <- bind_rows(lapply(estate_wrong, osm_search, key = "IqnuFYj5FkL3pgyGaj8fLQ2LdcGJBuJ4"))

# Replace geocodes in data frame
ir_est_df[ir_est_df$site =="Alumenda Estate", c("lon", "lat")] <- estate_geo[estate_geo$display_name == "Alimenda Estate, Miseu Folo, Southern Region, Malawi", c("lon", "lat")]
ir_est_df[ir_est_df$site =="Dwangwa Cane (Illovo)", c("lon", "lat")] <- estate_geo[estate_geo$display_name == "Dwangwa Estate, Dwangwa, Central Region, Malawi", c("lon", "lat")]
ir_est_df[ir_est_df$site =="Illovo Nchalo Estate", c("lon", "lat")] <- estate_geo[estate_geo$display_name == "Nchalo Sucoma Sugar Estate, Nchalo, Southern Region, Malawi", c("lon", "lat")]

# Map
leaflet() %>% 
  addTiles() %>% 
  addCircles(data = ir_est_df, lat = ~ lat, lng = ~ lon,  label = ~as.character(paste(site, actual_ha, sep = "_")),
             radius = ~ radius, weight = 1, fillOpacity = 0.1) %>%
  addMarkers(data = estate_geo, lat = ~ lat, lng = ~ lon)

# Additional
check <- c("Kavuzi", "kawalazi")
check_geo <- bind_rows(lapply(check, osm_search, key = "IqnuFYj5FkL3pgyGaj8fLQ2LdcGJBuJ4"))


### PROCESS DISTRICT IRRIGATION DATA
# Function to read sheets
multi_sheet_f <- function(sh, sheet){
  print(sh)
  df <- read_excel(file, sheet = sh) %>%
    mutate(EASTING = as.character(EASTING),
           NORTHING = as.character(NORTHING),
           adm2 = sh)
}  

# File
file <- file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/Other/Irrigation/District_irrigation_raw.xlsx")

# get sheet names
wb <- loadWorkbook(file)
sheets <- getSheets(wb)

# Read and bind
ir_dist_raw <- bind_rows(lapply(sheets, multi_sheet_f, file)) %>%
  rename(site = `SITE NAME`, potential_ha = `POTENTIAL HA`, actual_ha = `ACTUAL HA`, easting = EASTING,
         northing = NORTHING, status = `FORMAL/ INFORMAL`, technology = TECHNOLOGY, operation = OPERATION)

# Mzimba correction
# obs No 295-298 of MZIMBA are in degrees - converted to UTM

# Convert degrees to decimals
mzimba_cor <- filter(ir_dist_raw, adm2 == "Mzimba" & No %in% c(295:298)) %>%
  mutate(lat = as.numeric(conv_unit(northing, from = 'deg_dec_min', to = 'dec_deg')),
         lon = as.numeric(conv_unit(easting, from = 'deg_dec_min', to = 'dec_deg'))) %>%
  dplyr::select(-northing, -easting) %>%
  rename(northing = lat, easting = lon)

# Add standard projection
check_coord <- mzimba_cor %>%
  dplyr::select(easting, northing)
coordinates(check_coord) <- c("easting", "northing")
proj4string(check_coord) <- crs

# Plot => OK
plot(adm2)
plot(check_coord, add = T, col = "red")                       

leaflet(data = check_coord) %>% 
  addTiles() %>% 
  addCircles()

# Reproject
check_coord <- spTransform(check_coord, crs_ir) %>%
  as.data.frame()

# Replace coordinates
mzimba_cor$easting <- check_coord$easting
mzimba_cor$northing <- check_coord$northing

# Create new file and update
ir_dist <- filter(ir_dist_raw, !(adm2 == "Mzimba" & No %in% c(295:298))) %>%
  mutate(northing = as.numeric(northing),
         easting = as.numeric(easting)) %>%
  bind_rows(.,mzimba_cor)

# Change names and add ID for easy referencing
ir_dist <- ir_dist %>%
  mutate(ID = c(1: nrow(.))) %>%
  rename()

# Add projection to points and reproject to WSG84
ir_dist_geo <- ir_dist %>%
  filter(!is.na(easting) & !is.na(northing)) %>%
  mutate(lat = northing, lon = easting) %>%
  as.data.frame()
coordinates(ir_dist_geo) <- c("lon", "lat")
proj4string(ir_dist_geo) <- CRS(crs_ir)
ir_dist_geo <- spTransform(ir_dist_geo, crs)

# Create data frame with all info and add radius needed to create circle with same same as actual_ha
# A = pi x r2 => r = sqrt(A/pi)
ir_dist_df <- as.data.frame((ir_dist_geo)) %>%
  mutate(radius = sqrt(actual_ha*10000/pi))


### CHECK POINTS OUTSIDE MALAWI
# 37 points outside Malawi area, probably because of data errors. 
# All checked and these cannot be corrected.
ir_dist_in_MWI <- as.data.frame(ir_dist_geo[adm2, ])
ir_dist_out_MWI <- filter(ir_dist_df, !(ID %in% ir_dist_in_MWI$ID))

# Filter out observations with wrong coords
ir_dist_df <- ir_dist_df %>%
  filter(ID %in% ir_dist_in_MWI$ID) %>%
  mutate(type = "District")

leaflet() %>% 
  addTiles() %>% 
  addCircles(data = ir_dist_df, lat = ~ lat, lng = ~ lon,  label = ~as.character(paste(site, actual_ha, sep = "_")),
             radius = ~ radius, weight = 1, fillOpacity = 0.1) %>%
  addRasterImage(GMIA, colors = pal, opacity = 0.5) %>%
  addLegend(values = values(GMIA), pal = pal, 
            title = "irrigated area")
  

### ADD ADDITIONAL SCHEMES FROM WORLD BANK REPORT
# Two estates to presented in irrigation plan
# Identified by means of osm and google (none of them included them both)
kavuzi <- osm_search("kavuzi", key = "IqnuFYj5FkL3pgyGaj8fLQ2LdcGJBuJ4") %>%
  dplyr::select(lat, lon) %>%
  mutate(site = "Kavuzi tea estate",
         actual_ha = 810)

kawalazi <- geocode("Kawalazi tea estate") %>%
  mutate(site = "Kawalazi tea estate",
         actual_ha = 670)

ir_add <- bind_rows(kavuzi, kawalazi) %>%
  mutate(type = "Estate",
         radius = sqrt(actual_ha*10000/pi),
         status = "Formal",
         adm2 = "Nkhata Bay",
         remarks = "Tea")

### COMBINE DATA
ir_df <- bind_rows(ir_dist_df, ir_est_df, ir_add)

# plot
leaflet(adm2) %>% 
  addTiles() %>% 
  addCircles(data = ir_df, lat = ~ lat, lng = ~ lon,  label = ~as.character(paste(site, actual_ha, sep = "_")),
             radius = ~ radius, weight = 1, fillOpacity = 0.1) %>%
  addRasterImage(GMIA, colors = pal, opacity = 0.5) %>%
  addLegend(values = values(GMIA), pal = pal, 
            title = "irrigated area") %>%
  addMarkers(data = ir_add, lat = ~ lat, lng = ~ lon) 

# save
write_csv(ir_df, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/irrigation_MWI.csv"), na = "")
