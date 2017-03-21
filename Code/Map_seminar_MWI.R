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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### ADMINISTRATIVE REGIONS MAP
# MWI GADM
MWI_adm1 <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm1.rds"))
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
districts <- MWI_adm1_for %>%
            group_by(NAME_1) %>%
            summarize(long = mean(long),
                      lat = mean(lat))
districts <- as.data.frame(coordinates(MWI_adm1)) %>%
  mutate(name = MWI_adm1_df$NAME_1) %>%
  set_names(c("long", "lat", "name"))


# Draw map
Fig_adm1 <- ggplot() +
  geom_polygon(data = MWI_adm1_for, aes(x = long, y = lat, fill = NAME_1), colour = "black") +
  geom_point(data = MWI_city, aes(x = long, y = lat), colour = "black") +
  geom_text(data = districts, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(33)) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

Fig_adm1
ggsave("FigTabMap/Fig_adm1.png")

### LAND COVER MAP
# Load SIMU Map
SIMU_5min_poly <- readOGR(file.path(dataPath, "GLOBIOM/simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 454 = Malawi)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==454,]

# Load land cover map
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\MWI\\Spatial_data\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 2\\malawi 2000 classification scheme2.img"))

# Load GADM country borders
country_map_raw <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm0.rds"))

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
source(file.path(root, "Code/MWI/household_surveys/Location_MWI_2010.r"))
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

