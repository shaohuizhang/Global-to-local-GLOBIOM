#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to illustrate crop distribution methodology
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if (!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("ggthemes")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm1_2000_", iso3c_sel, ".rds")))
adm0 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm0_2000_", iso3c_sel, ".rds")))

# Lu adm
lu_adm <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Roads
roads <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/roads/roads_", iso3c_sel, ".rds")))

# Detailed spatial data
sugc <- readOGR(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Schemes/zambia_sugc_2000.kml")))
whea <- readOGR(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Schemes/munkumpu_whea_2000.kml")))

# Zambezi map
zambezi_map <- readRDS(file.path(dataPath, "Data/Zambezi/Processed/Maps/zamcom_zambezi.rds"))

# esa land cover
lc_esa <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))

# Load esa legend
esa_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv"))

# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# IWMI
iwmi_raw <- raster(file.path(dataPath, "Data/Global/IWMI/IRRA_Africa_2010_v1_1c/irra_africa_2010_v1_1.img")) 
irra_africa_class <- levels(iwmi_raw)[[1]] %>%
  mutate(Class_Names = as.character(Class_Names),
         Class_Names = ifelse(ID == 0, "No crop", Class_Names))
crs(iwmi_raw) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


# Africa map
data("wrld_simpl")
wrld_simp_df <- wrld_simpl@data %>%
  rename(iso3c = ISO3) %>%
  mutate(continent = countrycode(iso3c, "iso3c", "continent"))

# Select Africa
wrld_simpl@data <- wrld_simp_df
africa <- wrld_simpl[wrld_simpl$continent %in% c("Africa"), ]

# Plot
ggplot() +
  geom_path(data = africa, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = zambezi_map, aes(x = long, y = lat, group = group), fill = "grey70", colour = "black") +
  geom_polygon(data = africa[africa$iso3c %in% c("MWI", "ZMB"),], aes(x = long, y = lat, group = group, fill = id), colour = "black") +
  coord_map() +
  theme_map() +
  guides(fill = F) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


### ZMB AND MWI MAP
# Load
mwi <- getData("GADM", country = "MWI", level = 0)
zmb <- getData("GADM", country = "zmb", level = 0)

# Combine
mwi_zmb <- rbind(mwi, zmb, makeUniqueIDs = TRUE)

# Crop land cover map
#lc_mwi_zmb <- crop(lc_esa, mwi_zmb)
#lc_mwi_zmb <- mask(lc_mwi_zmb, mwi_zmb)
#saveRDS(lc_mwi_zmb, "Cache/lc_mwi_zmb.rds")
lc_mwi_zmb <- readRDS("Cache/lc_mwi_zmb.rds")

# Create named vector for colours
esa_legend <- esa_legend %>%
  mutate(colour= rgb(R, G, B, max = 255)) %>%
  unique()

lc_colors <- esa_legend$colour
names(lc_colors) <- esa_legend$lc_code

# Create dataframe
lc_mwi_zmb_df <- as.data.frame(rasterToPoints(lc_mwi_zmb)) %>%
  setNames(c("x", "y","lc_code")) %>% 
  left_join(esa_legend)

# Plot
ggplot()+
  geom_polygon(data = zambezi_map, aes(x = long, y = lat, group = group), fill = "grey70", colour = "black") +
  geom_raster(data = lc_mwi_zmb_df, aes(x = x, y = y, fill = as.factor(lc_code))) +
  geom_path(data = zambezi_map, aes(x = long, y = lat, group = group), colour = "black") +
  geom_path(data = mwi_zmb, aes(x = long, y = lat, group = group), size = 1.5) +
  #geom_path(data = wrld_simpl, aes(x = long, y = lat, group = group)) +
  scale_fill_manual(values = lc_colors) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank()) 


### CREATE COUNTRY GRID
# NB method to first assign gridID numbers to global raster and then crop does not result in unique gridID?
r <- raster() # 1 degree raster
r <- disaggregate(r, fact=4) # 5 arcmin raster
grid <- crop(r, adm1)
values(grid) <- 1:ncell(grid) # Add ID numbers
names(grid) <- "gridID" 
grid <- mask(grid, adm1)
grid
plot(grid)

# Create polygon
grid_p <- rasterToPolygons(grid)
plot(grid_p) # Plot (with capital) from quickPlot package works faster but might still take time!


### PLOTS
# 1. adm
plot(adm0, lwd = 2)

# Set crop colors
crops_rank <- filter(lu_adm, adm == iso3c_sel) %>%
  arrange(desc(value))
crops <- unique(crops_rank$short_name)
col <- rainbow(length(crops))
names(col) <- crops


# lu_sy graph
lu_sy_df <- filter(lu_sy, system %in% c("I", "H")) %>%
  mutate(system = ifelse(system == "I", "Irrigated", "High-input"))
ggplot() +
  geom_col(data = lu_sy_df, aes(x = reorder(short_name, -value), y = value, fill = short_name)) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = col) +
  labs(x = "", y = "", title = "Production systems") +
  #coord_flip() +
  facet_wrap(~system, drop = T, scales = "free") +
  guides(fill = F) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=30),
        axis.text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
             
# Faostat graph
faostat_df <- filter(lu_adm, adm == iso3c_sel) %>%
  top_n(n = 10, wt = value)
ggplot() +
  geom_col(data = faostat_df, aes(x = reorder(short_name, -value), y = value, fill = short_name)) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = col) +
  labs(x = "", y = "", title = "Crop area") +
  #coord_flip() +
  guides(fill = F) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=30),
        axis.text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. grid
plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)

# 3. crop cover
grid_df <- grid_p@data
set.seed(10)
crop_cover_df <- sample_n(grid_df, 400) %>%
  mutate(crop_cover = terrain.colors(12)[5])
crop_cover_df <- left_join(grid_df, crop_cover_df)
crop_cover <- grid_p
crop_cover@data <- crop_cover_df

plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)
plot(crop_cover, border = "grey40", col = crop_cover@data$crop_cover, add = T)

# 4. Crop statistics
# Maps
n_adm <- length(adm1$ADM1_NAME)
adm_color_df <- data.frame(ADM1_NAME = unique(adm1$ADM1_NAME), adm_cover = rainbow(n_adm))

adm_cover <- intersect(grid_p, adm1)
adm_cover_df <- adm_cover@data %>%
  left_join(crop_cover_df) %>%
  left_join(adm_color_df) %>%
  mutate(adm_cover = ifelse(!is.na(crop_cover), adm_cover, NA))
adm_cover@data <- adm_cover_df

plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)
plot(crop_cover, border = "grey10", col = crop_cover@data$crop_cover, add = T)
plot(adm1, add = T, lwd = 2)

plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)
plot(crop_cover, border = "grey10", col = crop_cover@data$crop_cover, add = T)
plot(adm_cover, border = "grey10", col = adm_cover_df$adm_cover, add = T)
plot(adm1, add = T, lwd = 2)

# Statistics total and per adm

plot_f <- function(adm_sel){
  df <- filter(lu_adm, adm == adm_sel) %>%
    top_n(n = 6, wt = value)
  p = ggplot() +
    geom_col(data = df, aes(x = reorder(short_name, -value), y = value, fill = short_name)) +
    scale_y_continuous(label = comma) +
    scale_fill_manual(values = col) +
    labs(x = "", title = unique(df$adm), y = "") +
    guides(fill = F) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=30),
          axis.text = element_text(size=20))
  p
}

lapply(unique(lu_adm$adm), plot_f)


# 4. Irrigated area
set.seed(10)
ir_cover_df <- filter(crop_cover_df, !is.na(crop_cover)) %>%
  sample_n(80) %>%
  mutate(ir = "dark blue")
ir_cover_df <- left_join(grid_df, ir_cover_df)
ir_cover <- grid_p
ir_cover@data <- ir_cover_df

plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)
plot(crop_cover, border = "grey10", col = crop_cover@data$crop_cover, add = T)
plot(adm_cover, border = "grey10", col = adm_cover_df$adm_cover, add = T)
plot(ir_cover, border = "grey10", col = ir_cover@data$ir, add = T)
plot(adm1, add = T, lwd = 2)


# 5. Detailed irrigated information
whea <- crop(grid_p, extent(27, 27.5, -13, -13.5))
sugc <- crop(grid_p, extent(27, 28, -16, -15))
sugc_df <- sugc@data
set.seed(15)
sugc_sel <- sample_n(sugc_df, 11)

plot(adm0, lwd = 2)
plot(grid_p, border = "grey40", add = T)
plot(crop_cover, border = "grey10", col = crop_cover@data$crop_cover, add = T)
plot(adm_cover, border = "grey10", col = adm_cover_df$adm_cover, add = T)
plot(ir_cover, border = "grey10", col = ir_cover@data$ir, add = T)
plot(sugc[sugc$gridID %in% sugc_sel$gridID,], add = T, col = "sienna4", border = "grey10", lwd = 1.5)
plot(whea, add = T, col = "sienna4", border = "grey10", lwd = 1.5)
plot(adm1, add = T, lwd = 2)


### IWMI IRRIGATION
# Crop and mask land cover map
iwmi <- crop(iwmi_raw, mwi_zmb)
iwmi <- mask(iwmi, mwi_zmb)

# Add attributes
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
iwmi <- ratify(iwmi)
rat <- levels(iwmi)[[1]] #get the values of the unique cell frot the attribute table
rat <- left_join(rat, irra_africa_class)

# Create colours for legend and sort in right order
iwmi_col <- rat %>%
  mutate(colour= rgb(Red, Green, Blue, alpha = Opacity, max = 255)) %>%
  unique()
iwmi_col <- iwmi_col[order(iwmi_col$Class_Names, decreasing = F),]

# Links levels
levels(iwmi) <- iwmi_col
levels(iwmi)
rm(rat)

### ADD WORLD MAP
worldmap <- map("world", fill = TRUE, plot = FALSE)

# Convert maps data to polygon (one for each country) and project
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
# select zambezi and surrounding countries
border_zambezi <- worldmap_poly[names(worldmap_poly) %in% c(zambezi_c, "Tanzania")]

# Legend
myKey <- list(text=list(lab=iwmi_col$Class_Names),
              rectangles=list(col = iwmi_col$colour),
              space='bottom',
              columns=2)
# Plot
levelplot(iwmi, att = 'Class_Names', col.regions = iwmi_col$colour, margin = F, colorkey = FALSE, key = myKey) +
  layer(sp.polygons(mwi_zmb, col = "black", fill = "grey", lwd = 2.5), under = T) +
  layer(sp.polygons(border_zambezi, col = "black", lwd = 1)) 

# Save map
saveRDS(iwmi, file.path(dataPath, "Data/Zambezi/Processed/Maps/imwi_Zambezi_2000.rds"))


### ASSESS AREA BY SYSTEM TYPE
# bar chart
col_bar <- iwmi_col$colour
names(col_bar) <- iwmi_col$Class_Names
iwmi_col %>%
  filter(Class_Names != "No crop") %>%
  mutate(share = COUNT/sum(COUNT)*100) %>%
  ggplot(.) +
  geom_col(aes(x = reorder(Class_Names, share), y = share, fill = Class_Names)) +
  scale_fill_manual(values = col_bar) +
  coord_flip() +
  theme_bw() +
  labs(y = "Crop cover class (%)", x = "") +
  guides(fill = "none") +
  theme(axis.text.y=element_blank())


