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

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/MWI/Set_country.R")

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### LOAD DATA
# Load results from land use allocation model
file <- file.path(dataPath, paste0("Model/", iso3c_sel, "/results/20180116_ent_approx_", iso3c_sel, "_2000.gdx"))
cd_raw <- rgdx.param(file, "Palloc", names = c("gridID", "system", "value"),  compress = T) %>%
  mutate(system = as.character(system),
         gridID = as.numeric(as.character(gridID)))

# Load country grid
grid_r <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_r) <- "gridID"
grid_p <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_p_", iso3c_sel, ".rds")))

# Adm map
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm2_2000_adj.rds")))

# City information
data(world.cities)
cities <- filter(world.cities, country.etc == country_sel, capital == 1)

# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Lu adm
lu_adm <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Lc  
lc <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# lc_ir
lc_ir <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_ir_2000_", iso3c_sel, ".rds"))) 


### PREPARE MAPs
# urban mask
urban_mask <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/urban_mask/urban_mask_", iso3c_sel, ".rds")))
urban_mask_sf <- st_as_sf(urban_mask)


## TO CODE: CHECKS TO RESULTS!
### COMPARE RESULTS WITH INPUT DATA
# Compare crop cover with allocation
check1 <- land_use_raw %>%
  rename(al = value) %>%
  group_by(gridID) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(lc,.) %>%
  #left_join(priors,.) %>%
  mutate(check = value-al) %>%
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
###################

### MAPS
# Add NA to missing values to show where there is no crop cover
cd <- cd_raw %>%
  spread(system, value, fill = NA) 

# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid_r))

cd <- cd %>%
  left_join(grid_df,.) %>%
  gather(system, value, -gridID, -x, -y)

# Add short_name
cd <- cd %>%
  mutate(short_name = substr(system,0,4),
         short_name = ifelse(system %in% c("teas_coff_H", "teas_coff_I"), "teas_coff", short_name),
         sy = str_sub(system,-1,-1))


# Plot function
plot_crop_raster_f <- function(sys){
  df <- filter(cd, system %in% sys)
  p = ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
    geom_path(data = adm, aes (x = long, y = lat, group = group), colour = "black") +
    facet_wrap(~system) +
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

plot_crop_raster_f(c("grou_S", "maiz_S"))


# plot all irrigated


fig <- cd %>%
  filter(sy %in% "I") %>%
  na.omit %>%
  ggplot(.) +
    geom_polygon(data = adm, aes (x = long, y = lat, group = group), colour = "black", fill = "light grey") +
    geom_raster(aes(x = x, y = y, fill = system)) +
    #scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
    #scale_fill_continuous(na.value = "light grey") +
    
    #facet_wrap(~system) +
    coord_quickmap() +
    labs(x="", y="", size = "#HH", fill = "Crop area (ha)") +
    theme_classic() +
    theme(line = element_blank(),
          axis.text = element_blank(),
          strip.background = element_rect(colour = NA, fill = NA))



