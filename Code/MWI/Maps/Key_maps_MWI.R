#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot", "ggthemes")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "maps")
# Additional packages
#p_load("WDI", "countrycode")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA
# Simu
simu2country_poly <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/simu_MWI.rds"))
spplot(simu2country_poly, "SimUID", colorkey=FALSE)

# GAUL
adm2_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
adm2_df <- adm2_map@data %>%
  mutate(id = row.names(.))
plot(adm2_map)

# land cover
land_cover_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/ESA_MWI_2000.rds"))

# Irrigation
ir_2000_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2000")
ir_2010_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2010")
ir_2000_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/irrigation_map_MWI_2000.rds"))
ir_2010_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/irrigation_map_MWI_2010.rds"))


### IRRIGATION MAP
# Prepare ir data
ir_2000 <- ir_2000_raw %>%
  gather(crop, value, -site:-type) %>%
  na.omit() %>%
  mutate(year = 2000)

ir_2010 <- ir_2010_raw %>%
  gather(crop, value, -site:-type) %>%
  na.omit() %>%
  mutate(year = 2010)

ir <- bind_rows(ir_2000, ir_2010)

# Prepare adm
adm2_for <- fortify(adm2_map)

# Map
map_ir <- ggplot() +
  geom_polygon(data = adm2_for, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = ir, aes(x = lon, y = lat, size = value, colour = crop), alpha = 0.5) +
  coord_equal() +
  labs(x="", y="",
       colour = "Crop", size = "Irrigated \n area (ha)") +
  theme_map() +
  facet_wrap(~ year) +
  theme(legend.position = c(1,.2),
    strip.background = element_rect(colour = NA, fill = NA),
    strip.text.x = element_text(size = 10, face="bold"))


### ADM MAP
# Capital
data(world.cities)
capital <- filter(world.cities, country.etc == "Malawi", capital == 1)

# Fortify polygon
adm2_for <- fortify(adm2_map) %>%
  left_join(adm2_df)

# Set colours
cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))

# Set distric labels
districts <- as.data.frame(coordinates(adm2_map)) %>%
  mutate(name = adm2_df$ADM2_NAME) %>%
  set_names(c("long", "lat", "name")) %>%
  filter(name != "Area under National Administration")

# Draw map
fig_adm2 <- ggplot() +
  geom_polygon(data = adm2_for, aes(x = long, y = lat, fill = ADM2_NAME, group = group), colour = "black") +
  geom_polygon(data =filter(adm2_for, id == 662), aes(x = long, y = lat), colour = "black", fill = "blue") +
  geom_point(data = capital, aes(x = long, y = lat), colour = "black") +
  geom_text(data = districts, aes(x = long, y = lat, label = name), size = 2) +
  scale_fill_manual(values = cols(length(adm2_df$ADM2_NAME))) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())

#ggsave("FigTabMap/fig_adm2_MWI.png")


### ESA LAND COVER MAP
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
  layer(sp.polygons(simu2country_poly, col = "black", lwd = 2)) +
  layer(sp.polygons(adm2_map, col = "red", lwd = 2))

# png(file = "FigTabMap/fig_MWI_lc_SIMU.png")
# fig_land_cover
# dev.off()


### LSMS MAP
# Get location data
source(file.path(root, "Code/MWI/Household_surveys/location_MWI_2010.r"))
location <- location %>%
  group_by(lat, lon) %>%
  summarize(n = n()) %>%
  mutate(size = 1)

fig_LSMS <- ggplot()+
  #geom_raster(data = MWI_GAEZ, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = adm2_for, aes(x = long, y = lat, group = group), fill = "grey", colour = "black") +
  geom_polygon(data =filter(adm2_for, id == 662), aes(x = long, y = lat), colour = "black", fill = "white") +
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


#ggsave("FigTabMap/fig_LSMS.png")


