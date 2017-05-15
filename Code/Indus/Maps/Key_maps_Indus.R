#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to prepare key maps for indus basin
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


### GAUL MAP
adm2_map <- readRDS(file.path(dataPath, "Data/indus/Processed/Maps/GAUL_indus_adm2_2000.rds"))
adm2_df <- adm2_map@data %>%
  mutate(id = row.names(.))


### LOAD LAND COVER MAP 
land_cover_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/ESA_MWI_2000.rds"))


### LOAD SIMU MAP
simu_indus <- readRDS(file.path(dataPath, "Data/indus/Processed/Maps/simu_indus.rds"))


### LOAD BASIN SHAPE FILES
indus_map <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\indus\\indus_hybas_lev3.shp"))


### LOAD LAND COVER MAP 
#http://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
#land_cover_map <- readRDS(file.path(dataPath, "Data/indus/Processed/Maps/ESA_indus_2000.rds"))
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
land_cover_map <- crop(land_cover_map_ESA, indus_map)

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

### indus COUNTRIES
# Added more countries because it is nicer for mapping
indus_c <- c("Pakistan", "India", "China", "Afghanistan", "Nepal", "Tajikistan", "Uzbekistan", "Turkmenistan")

### ADM MAP
# Capital
data(world.cities)
capital <- filter(world.cities, country.etc %in% indus_c, capital == 1)

# Fortify GAUL
adm2_for <- fortify(adm2_map) %>%
  left_join(adm2_df)

# Fortify basin
indus_map_for <- fortify(indus_map)

# Fortify simu
simu_indus_for <- fortify(simu_indus)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts <- as.data.frame(coordinates(adm2_map)) %>%
  mutate(name = adm2_df$ADM2_NAME) %>%
  set_names(c("long", "lat", "name")) %>%
  filter(name != "Area under National Administration")

bbox(simu_indus)
# Draw map
fig_adm2_indus <- ggplot() +
  geom_polygon(data = adm2_for, aes(x = long, y = lat, fill = ADM2_NAME, group = group), colour = "black") +
  #geom_point(data = capital, aes(x = long, y = lat), colour = "black") +
  #geom_text(data = districts, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(adm2_df$id))) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank()) +
  geom_path(data = indus_map_for, aes(x = long, y = lat, group = group), colour = "black", size =2) + 
  geom_path(data = simu_indus_for, aes(x = long, y = lat, group = group), colour = "black", size = 0.5) + 
  borders("world", c(indus_c), colour = "black", size = 1) +
  #coord_cartesian(xlim = c(10, 43), ylim = c(-21.5, -8)) # distorts somehow
  coord_map(xlim = c(63, 85), ylim = c(22, 38))

fig_adm2_indus
ggsave(file.path(root, "FigTabMap/fig_adm_indus.png"))


### COMBINE MAPS
# plot world map using map
worldmap <- map("world", fill = TRUE, plot = FALSE)

# Convert maps data to polygon (one for each country) and project
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
# select indus and surrounding countries
border_indus <- worldmap_poly[names(worldmap_poly) %in% c(indus_c, "Tanzania")]

# Plot map 
fig_land_cover_indus <- levelplot(land_cover_map, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
  layer(sp.polygons(indus_map, col = "black", lwd = 2.5)) +
  layer(sp.polygons(simu_indus, col = "black", lwd = 1)) +
  layer(sp.polygons(border_indus, col = "black", lwd = 2)) 

png(filename = file.path(root, "FigTabMap/fig_land_cover_indus.png"))
fig_land_cover_indus
dev.off()


