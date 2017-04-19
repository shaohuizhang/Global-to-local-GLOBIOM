#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "maps")
# Additional packages
#p_load("WDI", "countrycode")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM" 

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### ADMINISTRATIVE REGIONS MAP MWI
# MWI GADM
MWI_adm1 <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GADM_2.8_MWI_adm1.rds"))
MWI_adm1_df <- MWI_adm1@data %>%
  rename(id = OBJECTID) %>%
  mutate(id = as.character(id))

# Capital
data(world.cities)
MWI_city <- filter(world.cities, country.etc == "Malawi", capital == 1)

# Fortify polygon
MWI_adm1_for <- fortify(MWI_adm1) %>%
  left_join(MWI_adm1_df)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts_MWI <- as.data.frame(coordinates(MWI_adm1)) %>%
  mutate(name = MWI_adm1_df$NAME_1) %>%
  set_names(c("long", "lat", "name"))


# Draw map
Fig_adm1_MWI <- ggplot() +
  geom_polygon(data = MWI_adm1_for, aes(x = long, y = lat, fill = NAME_1), colour = "black") +
  geom_point(data = MWI_city, aes(x = long, y = lat), colour = "black") +
  geom_text(data = districts_MWI, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(MWI_adm1_df$NAME_1))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_adm1_MWI
ggsave("FigTabMap/Fig_adm1_MWI.png")

### ADMINISTRATIVE REGIONS MAP ZMB
# ZMB GADM
ZMB_adm1 <- readRDS(file.path(dataPath, "Data\\ZMB\\Processed\\Maps\\GADM_2.8_ZMB_adm1.rds"))
ZMB_adm1_df <- ZMB_adm1@data %>%
  rename(id = OBJECTID) %>%
  mutate(id = as.character(id))

# Capital
data(world.cities)
ZMB_city <- filter(world.cities, country.etc == "Zambia", capital == 1)

# Fortify polygon
ZMB_adm1_for <- fortify(ZMB_adm1) %>%
  left_join(ZMB_adm1_df)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts_ZMB <- as.data.frame(coordinates(ZMB_adm1)) %>%
  mutate(name = ZMB_adm1_df$NAME_1) %>%
  set_names(c("long", "lat", "name"))


# Draw map
Fig_adm1_ZMB <- ggplot() +
  geom_polygon(data = ZMB_adm1_for, aes(x = long, y = lat, fill = NAME_1), colour = "black") +
  geom_point(data = ZMB_city, aes(x = long, y = lat), colour = "black") +
  geom_text(data = districts_ZMB, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(ZMB_adm1_df$NAME_1))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_adm1_ZMB
ggsave("FigTabMap/Fig_adm1_ZMB.png")


### ADMINISTRATIVE REGIONS MAP ZWE
# ZWE GADM
ZWE_adm1 <- readRDS(file.path(dataPath, "Data\\ZWE\\Processed\\Maps\\GADM_2.8_ZWE_adm1.rds"))
ZWE_adm1_df <- ZWE_adm1@data %>%
  rename(id = OBJECTID) %>%
  mutate(id = as.character(id))

# Capital
data(world.cities)
ZWE_city <- filter(world.cities, country.etc == "Zimbabwe", capital == 1)

# Fortify polygon
ZWE_adm1_for <- fortify(ZWE_adm1) %>%
  left_join(ZWE_adm1_df)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts_ZWE <- as.data.frame(coordinates(ZWE_adm1)) %>%
  mutate(name = ZWE_adm1_df$NAME_1) %>%
  set_names(c("long", "lat", "name"))


# Draw map
Fig_adm1_ZWE <- ggplot() +
  geom_polygon(data = ZWE_adm1_for, aes(x = long, y = lat, fill = NAME_1), colour = "black") +
  geom_point(data = ZWE_city, aes(x = long, y = lat), colour = "black") +
  geom_text(data = districts_ZWE, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(ZWE_adm1_df$NAME_1))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_adm1_ZWE
ggsave("FigTabMap/Fig_adm1_ZWE.png")

### LAND COVER MAP ZMB
# Load SIMU Map
SIMU_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 454 = Malawi)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==454,]

# Load land cover map
land_cover_map_raw <- raster(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 2\\malawi 2000 classification scheme2.img"))

# Load GADM country borders
country_map_raw <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GADM_2.8_MWI_adm0.rds"))

# Reproject GADM and SIMU maps
country_crs <- crs(land_cover_map_raw)
SIMU2country_poly_rp <- spTransform(SIMU2country_poly, country_crs)
country_map_rp <-  spTransform(country_map_raw, country_crs)

# Create map
Fig_MWI_lc_SIMU <- levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme) +
  layer(sp.polygons(SIMU2country_poly_rp, col = "black")) +
  layer(sp.polygons(country_map_rp, col = "black"))

png(file = "FigTabMap/Fig_MWI_lc_SIMU.png")
Fig_MWI_lc_SIMU
dev.off()

# # Raster to point
# land_cover_map_df <- rasterToPoints(land_cover_map_raw)

### LSMS MAP
# Get location data
source(file.path(root, "Code/MWI/Household_surveys/location_MWI_2010.r"))
location <- location %>%
  group_by(lat, lon) %>%
  summarize(n = n()) %>%
  mutate(size = 1)

Fig_LSMS <- ggplot()+
  #geom_raster(data = MWI_GAEZ, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = MWI_adm1_for, aes(x = long, y = lat, group = group), fill = "grey", colour = "black") +
  geom_point(data = location, aes(x = lon, y = lat, size = size), colour = "red") +
  scale_size_continuous(name = "", range = c(1, 1), labels = "Enumeration areas") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  #scale_fill_gradient(low = "light green", high = "dark green") +
  #scale_fill_distiller(palette = "Spectral", name = "Potential\nyield (tons/ha)") +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_LSMS
ggsave("FigTabMap/Fig_LSMS.png")


### LAND COVER MAP ZMB
# Load SIMU Map
SIMU_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 894 = Zambia)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==894,]

# Load land cover map
land_cover_map_raw <- raster(file.path(dataPath, "Data/ZMB\\Raw\\Spatial_data\\Zambia_LandCover_2000_Scheme_II\\Zambia_Landcover2_2000_Scheme_II.tif"))

# Load GADM country borders
country_map_raw <- readRDS(file.path(dataPath, "Data\\ZMB\\Processed\\Maps\\GADM_2.8_ZMB_adm0.rds"))

# Create map
Fig_ZMB_lc_SIMU <- levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme) +
  layer(sp.polygons(SIMU2country_poly, col = "black")) +
  layer(sp.polygons(country_map_raw, col = "black"))

png(file = "FigTabMap/Fig_ZMB_lc_SIMU.png")
Fig_ZMB_lc_SIMU
dev.off()

### LAND COVER MAP ZWE
# Load SIMU Map
SIMU_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 716 = Zimbabwe)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==716,]

# Load land cover map
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"))

# Load GADM country borders
country_map <- readRDS(file.path(dataPath, "Data\\ZWE\\Processed\\Maps/GAUL_ZWE_adm1_2000.rds"))


### PREPARE COUNTRY LAND COVER MAP
land_cover_map_ZWE <- crop(land_cover_map_ESA, country_map)
land_cover_map_ZWE <- mask(land_cover_map_ZWE, country_map)
levelplot(land_cover_map_ZWE, par.settings = RdBuTheme)

# Add attributes
# Load ESA legend
ESA_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv"))


# Add attributes
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
land_cover_map_ZWE <- ratify(land_cover_map_ZWE)
rat <- levels(land_cover_map_ZWE)[[1]] #get the values of the unique cell frot the attribute table
rat <- left_join(rat, ESA_legend)

# Create colours for legend and sort in right order
ESA_colour <- rat
ESA_colour <- ESA_colour %>%
  filter(ID %in% seq(0, 220, 10)) %>%
  mutate(colour= rgb(R, G, B, max = 255)) %>%
  unique()
ESA_colour <- ESA_colour[order(ESA_colour$land_cover_short, decreasing = F),]

# Links levels
levels(land_cover_map_ZWE) <- rat
levels(land_cover_map_ZWE)
rm(rat)

# Create map
Fig_ZWE_lc_SIMU <- levelplot(land_cover_map_ZWE, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
  layer(sp.polygons(SIMU2country_poly, col = "black")) +
  layer(sp.polygons(country_map, col = "black"))

  
  levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme) +
  layer(sp.polygons(SIMU2country_poly, col = "black")) +
  layer(sp.polygons(country_map_raw, col = "black"))

png(file = "FigTabMap/Fig_ZWE_lc_SIMU.png")
Fig_ZWE_lc_SIMU
dev.off()


# # Raster to point
# land_cover_map_df <- rasterToPoints(land_cover_map_raw)

### LSMS MAP
# Get location data
source(file.path(root, "Code/MWI/Household_surveys/location_MWI_2010.r"))
location <- location %>%
  group_by(lat, lon) %>%
  summarize(n = n()) %>%
  mutate(size = 1)

Fig_LSMS_MWI <- ggplot()+
  #geom_raster(data = MWI_GAEZ, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = MWI_adm1_for, aes(x = long, y = lat, group = group), fill = "grey", colour = "black") +
  geom_point(data = location, aes(x = lon, y = lat, size = size), colour = "red") +
  scale_size_continuous(name = "", range = c(1, 1), labels = "Enumeration areas") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  #scale_fill_gradient(low = "light green", high = "dark green") +
  #scale_fill_distiller(palette = "Spectral", name = "Potential\nyield (tons/ha)") +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_LSMS_MWI
ggsave("FigTabMap/Fig_LSMS_MWI.png")


### ZIMBABWE LAND COVER MAP


### ZIMBABWE SURVEY MAP




