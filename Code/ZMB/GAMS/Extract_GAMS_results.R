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
p_load("countrycode", "gdxrrw", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### LOAD DATA
# Load results from land use allocation model
file <- file.path(dataPath, "Model/Results/land_use.gdx")
land_use_raw <- rgdx.param(file, "Palloc", names = c("gridID", "system", "value"),  compress = T) %>%
  mutate(system = as.character(system),
         gridID = as.numeric(as.character(gridID)))

# Load country grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Adm map
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))

# City information
data(world.cities)
cities <- filter(world.cities, country.etc == "Malawi", capital == 1)

# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_2000_MWI.rds"))

# Agricultural statistics
ag_stat_2000 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))

# Priors
priors <- read_csv(file.path(dataPath, "Model/Data/priors.csv"), col_names = c("gridID", "short_name", "prior")) %>%
  spread(short_name, prior)

### PREPARE MAPs
# Transform to sf for easy plotting with ggplot
grid_sf <- st_as_sf(grid)


# urban mask
urban_mask <- readRDS(file.path(dataPath, "Data/MWI/Processed/Spatial_data/urban_mask_MWI.rds"))
urban_mask_sf <- st_as_sf(urban_mask)


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


### MAPS
# Add NA to missing values to show where there is no crop cover
land_use <- land_use_raw %>%
  spread(system, value, fill = NA) 

# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid_r))

land_use <- land_use %>%
  left_join(grid_df,.) %>%
  gather(system, value, -gridID, -x, -y)

# Add short_name
land_use <- land_use %>%
  mutate(short_name = substr(system,0,4),
         short_name = ifelse(system %in% c("teas_coff_H", "teas_coff_I"), "teas_coff", short_name))

# Plot function
plot_crop_raster_f <- function(crop){
  df <- filter(land_use, short_name %in% crop)
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
    geom_path(data = adm2, aes (x = long, y = lat, group = group), colour = "black") +
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


plot_crop_raster_f(c("cass", "maiz"))



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
