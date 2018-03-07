#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to extract GAMS results and map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("countrycode", "gdxrrw", "plotKML", "viridis")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### LINK GAMS LIBRARIES
igdx(GAMSPath)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Load results from land use allocation model
file <- file.path(dataPath, paste0("Model/", iso3c_sel, "/Results/min_entropy_ZMB_2000.gdx"))
lu_raw <- rgdx.param(file, "Palloc", names = c("gridID", "sy", "value"),  compress = T) %>%
  mutate(sy = as.character(sy),
         gridID = as.numeric(as.character(gridID)))

# Adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# City information
data(world.cities)
cities <- filter(world.cities, country.etc == country_sel, capital == 1)

# Lc  
lc <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Priors
priors <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/priors_2000_", iso3c_sel, ".rds"))) 

# urban mask
#urban_mask <- readRDS(file.path(dataPath, "Data/MWI/Processed/Spatial_data/urban_mask_MWI.rds"))


### MAPS
# Add NA to missing values to show where there is no crop cover
lu <- lu_raw %>%
  spread(sy, value, fill = NA) 

# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid))

lu <- lu %>%
  left_join(grid_df,.) %>%
  gather(sy, value, -gridID, -x, -y)

# Add short_name and system
lu <- lu %>%
  mutate(short_name = substr(sy, 0, 4),
         system = str_sub(sy, start = -1))

# Plot function
plot_crop_raster_f <- function(crop, sys){
  df <- filter(lu, short_name %in% crop, system %in% sys)
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
    geom_path(data = adm, aes (x = long, y = lat, group = group), colour = "black") +
    facet_wrap(~short_name) +
    coord_quickmap() +
    labs(x="", y="", size = "#HH", fill = "Crop area (ha)") +
    theme_classic() +
    theme(line = element_blank(),
          axis.text = element_blank(),
          strip.background = element_rect(colour = NA, fill = NA)) +
    geom_point(data = cities, aes(x = long, y = lat), col = "black") +
    geom_text(data = cities, aes(x = long, y = lat, label = name), size = 4)
  
  p
}

plot_crop_raster_f("maiz", "S")
plot_crop_raster_f("whea", "I")
plot_crop_raster_f("cass", "S")

# Plot all
df <- filter(lu, short_name %in% c("maiz", "whea", "cass"))
ggplot() +
  geom_raster(data = lu, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
  geom_path(data = adm, aes (x = long, y = lat, group = group), colour = "black") +
  facet_wrap(~short_name, scales = "free") +
  #coord_quickmap() +
  coord_equal() +
  labs(x="", y="", size = "#HH", fill = "Crop area (ha)") +
  theme_classic() +
  theme(line = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(colour = NA, fill = NA)) +
  guides(fill = F)




# SF MAP NEED GEOM_SF
# # Prepare map
# grid_sf <- grid_sf %>%
#   left_join(.,land_use)
# 
# # Map
# ggplot(grid_sf) + geom_sf(aes(fill = maiz)) +
#   scale_fill_gradient(low="grey",high="red", na.value = "blue")
# 
# 
# grid_sf2 <- grid_sf %>%
#   gather(short_name, value, -geometry, -gridID) %>%
#   filter(short_name %in% c("maiz", "grou", "cass"))
# ggplot(grid_sf2) + geom_sf(aes(fill = value)) + 
#   facet_grid(. ~ short_name) +
#   scale_fill_gradient(low="grey",high="red", na.value = "blue")


### COMPARE RESULTS WITH INPUT DATA
# Compare crop cover with allocation
check1 <- land_use_raw %>%
  group_by(gridID) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(crop_cover,.) %>%
  left_join(priors,.) %>%
  mutate(check = area-value) %>%
  arrange(check)

# Select gridID with difference >  -1000
grid_check <- filter(check1, check < -1000)

# Plot outliers
plot(grid, col = NULL)
plot(grid[grid$gridID %in% grid_check$gridID,], col = "red", add = T)
grid_check_p <- grid[grid$gridID %in% grid_check$gridID,]

ggplot(grid_sf) + 
  geom_sf() + 
  geom_sf(data = filter(grid_sf, gridID %in% grid_check$gridID), fill = "red") +
  geom_sf(data = urban_mask_sf, fill = "blue", alpha = 0.5) +
  geom_point(data = cities, aes(x = long, y = lat), col = "green") +
  geom_text(data = cities, aes(x = long, y = lat, label = name))

plotKML(grid_check_p)

# Compare total crop area with allocation
ag_stat_adm0 <- ag_stat_2000 %>%
  filter(adm_level == 0) %>%
  rename(stat = value)

check2 <- land_use_raw %>%
  group_by(short_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(ag_stat_adm0,.) %>%
  mutate(check = stat - value) %>% 
  arrange(desc(check))

# Compare adm2 area with allocation
ag_stat_adm2 <- ag_stat_2000 %>%
  filter(adm_level == 2) %>%
  rename(stat = value, adm2 = adm)

check3a <- land_use_raw %>%
  left_join(crop_cover,.) %>%
  group_by(adm2, short_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(ag_stat_adm2,.) %>%
  mutate(check = (value-stat),
         share = ((value-stat)/stat)*100) %>% 
  arrange(desc(check))


check3b <- check3 %>%
  ungroup() %>%
  group_by(adm2) %>%
  summarize(value = sum(value, na.rm = T),
            stat = sum(stat, na.rm = T)) %>%
  mutate(check = (value-stat),
         share = ((value-stat)/stat)*100) %>% 
  arrange(desc(check))
