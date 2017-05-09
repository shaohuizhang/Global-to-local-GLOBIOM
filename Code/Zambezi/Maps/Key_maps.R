#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to prepare key maps at basin level
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
zambezi_map <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\Zambezi\\Zambezi_hybas_lev3.shp"))
plotKML(zambezi_map)

### LOAD LAND COVER MAP 
#http://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
land_cover_map <- crop(land_cover_map_ESA, zambezi_map)
land_cover_map <- mask(land_cover_map, zambezi_map)
plot(land_cover_map)

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
fig_land_cover <- levelplot(land_cover_map, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
  layer(sp.polygons(zambezi_map, col = "black", lwd = 2)) +
  layer(sp.polygons(simu_5min_poly, col = "red", lwd = 2)) 

fig_land_cover



land_cover_for <- as.data.frame(rasterToPoints(land_cover_map))

ggplot() +
geom_tile(data = land_cover_for, aes(x=x, y=y, fill=ESACCI.LC.L4.LCCS.Map.300m.P1Y.2000.v2.0.7), alpha=0.8) 
### WORLD MAP IN MAPS PACKAGE
# plot world map using map
worldmap <- map("world", fill = TRUE, plot = FALSE)
map(worldmap)


# Convert maps data to polygon (one for each country) and project
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(worldmap_poly)
names(worldmap_poly)

zam_c_poly <- worldmap_poly[worldmap_poly[worldmap_poly$ADM0_NAME == "Malawi",]]

str(worldmap_poly)[[1]]
### DOWNLOAD GAUL

zwe <- worldmap_poly[c("Zimbabwe")]
plot(zwe)

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
