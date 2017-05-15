#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare overlay simu with basin map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD SIMU MAPS
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"))
simu_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")
simu_raster <- raster(file.path(dataPath, "Data/GLOBIOM/simu_raster/w001001.adf"))

### DOWNLOAD BASIN SHAPE FILES
zambezi <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\Zambezi\\Zambezi_hybas_lev3.shp"))
indus <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\Indus\\Indus_hybas_lev3.shp"))
plotKML(zambezi)
plotKML(indus)


  



### WORLD MAP IN MAPS PACKAGE
# plot world map using map
worldmap <- map("world", fill = TRUE, plot = FALSE)
map(worldmap)

# plot world map using ggplot
# quick and dirty using borders as detail of border is limited
# For more detail download GADM data
worldborders <- borders("world", fill = "grey", colour = "blue")
ggplot() + worldborders

worldborders <- borders("world", c("India", "Pakistan"), fill = "grey", colour = "blue")
ggplot() + worldborders

# Other option using map_data which creates a dataframe
worldmap2 <- map_data("world")
ggplot() + geom_polygon(data = worldmap2,  aes(x=long, y = lat, group = group))

# Convert maps data to polygon (one for each country) and project
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(worldmap_poly)
names(worldmap_poly)

names(world)


### DOWNLOAD GAUL




# GAUL adm1
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"))
GAUL_adm1_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")
zambezi_c_adm1 <- GAUL_adm1_2000[GAUL_adm1_2000$ADM0_NAME %in% zambezi_c,]
plot(zambezi_c_adm1)

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
GAUL_adm2_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")

# Gaul adm1
GAUL_MWI_adm1_2000 <- GAUL_adm1_2000[GAUL_adm1_2000$ADM0_NAME == "Malawi",]
plot(GAUL_MWI_adm1_2000)

# Gaul adm2
GAUL_MWI_adm2_2000 <- GAUL_adm2_2000[GAUL_adm2_2000$ADM0_NAME == "Malawi",]
plot(GAUL_MWI_adm2_2000)


### USE ZAMBEZI MAP TO CLIP SIMU MAP
# Create polygon
zambezi_simu_poly <- gIntersection(zambezi, simu_5min_poly, byid = TRUE, drop_lower_td = TRUE)
plot(zambezi_simu_poly, col = "lightblue")

# Extract SIMU IDS
zambezi_simu <- crop(simu_raster, zambezi)
zambezi_simu <- mask(zambezi_simu, zambezi)
plot(zambezi_simu)
plot(zambezi, add = T)
zambezi_simu_df <- as.data.frame(rasterToPoints(zambezi_simu)) %>%
  setNames(c("x", "y", "SimUID")) %>%
  filter(SimUID != 0)

# Check selection
check <- SIMU_5min_poly[SIMU_5min_poly$SimUID %in% zambezi_simu_df$SimUID,]
plot(zambezi, add = T)
rm(check)

### USE INDUS MAP TO CLIP SIMU MAP
# Create polygon
indus_simu_poly <- gIntersection(indus, simu_5min_poly, byid = TRUE, drop_lower_td = TRUE)
plot(indus_simu_poly, col = "lightblue")
plot(indus_simu_poly)

# Extract SIMU IDS
indus_simu <- crop(simu_raster, indus)
indus_simu <- mask(indus_simu, indus)
plot(indus_simu)
plot(indus, add = T)
indus_simu_df <- as.data.frame(rasterToPoints(indus_simu)) %>%
  setNames(c("x", "y", "SimUID")) %>%
  filter(SimUID != 0)

# Check selection
check <- simu_5min_poly[simu_5min_poly$SimUID %in% indus_simu_df$SimUID,]
plot(check, add = T)
rm(check)


### ZAMBEZI MAPS
zambezi_c <- c("Malawi", "Botswana", "Zambia", "Zimbabwe", "Mozambique","Angola", "Tanzania", "Namibia", 
               "Democratic Republic of the Congo", "South Africa")
indus_c <- c("")

zambezi_f <- fortify(zambezi)

ggplot() + borders("world", zambezi_c, fill = "grey", colour = "blue") +
  geom_path(data = zambezi_f, aes(x = long, y = lat, group = group), colour = "red")

data(maps:wordMapEnv)
map('usa')
map('rivers')
, add=TRUE)
