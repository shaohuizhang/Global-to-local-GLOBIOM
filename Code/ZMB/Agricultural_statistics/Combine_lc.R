#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to create synergistic land cover map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "plotKML", "sf")


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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD DATA
# Adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# Adm_r
adm_r <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_r_2000_", iso3c_sel, ".rds"))) %>%
  dplyr::select(-x, -y)


# lc data
esa_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_ESA_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "esa")
rcmrd_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_RCMRD_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "rcmrd")
glc_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_glc2000_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "glc2000")

# # gia
# gia_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gia/gia_", iso3c_sel, ".tif")))
# names(gia_raw) <- "gia"

# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Agricultural statistics
lu_adm_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ag_stat_2000_", iso3c_sel, ".csv"))) 

# Population map
pop <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/pop/pop_30sec_", iso3c_sel, ".tif")))
names(pop) <- "pop"

# Travel time map
tt <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/travel_time/travel_time_", iso3c_sel, ".tif")))
names(tt) <- "travel_time"

# Roads
roads <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/roads/roads_", iso3c_sel, ".rds")))


### COMBINE DIFFERENT CLASSES OF LC DATA
# Here also crop specific land cover maps should be added and be given the lowest rank so they are always included
# in the final land cover map.

esa <- esa_raw %>%
  group_by(gridID, source) %>%
  summarize(value = sum(value, na.rm = T))

rcmrd <- rcmrd_raw %>%
  dplyr::select(gridID, value, source)

glc <- glc_raw %>%
  dplyr::select(gridID, value, source)

lc_comb <- bind_rows(esa, rcmrd, glc)


### RANK GRIDID
# CODE NEED TO be replaced by a piece of code where a ranking table is loaded to make it more general. 
# Data for different land cover maps are combined to create a synergistic land cover map (Fritz et al. 2011)
# We use the following ranking:
# 1. Grid with crop cover according to all sources
# 2. Grid with crop cover from rcmrd + esa
# 3. Grid with crop cover from rcmrd + glc2000
# 4. Grid with crop cover from esa + glc2000
# rcmrd has our preference because it has the highest resolution followed by esa and glc 2000

# Function to rank cells
rank_f <- function(df){
  cov = sum(!is.na(df$value))  
  if(cov == 3){
    rnk <- 1  
  } else if (all(c("rcmrd", "esa") %in% df$source)){
    rnk <- 2
  } else if (all(c("rcmrd", "glc2000") %in% df$source)){
    rnk <- 3
  } else if (c("rcmrd") %in% df$source){
    rnk <- 4
  } else 
    rnk <- 5
  ranking <- data.frame(coverage = cov, rank = rnk)
  return(ranking)
}

# Rank
ranking <- lc_comb %>%
  group_by(gridID, x, y) %>%
  do(rank_f(.))

# Rank statistics
# Number of cells
rank_stat <- lc_comb %>%
  spread(source, value) %>%
  left_join(ranking)
table(rank_stat$rank)

# Plot ranking
ggplot() +
  geom_raster(data = rank_stat, aes(x = x, y = y, fill = factor(rank))) +
  geom_path(data = adm, aes(x = long, y = lat, group = group), colour = "black") +
  geom_path(data = roads, aes(x = long, y = lat, group = group), colour = "grey50") +
  coord_equal() +
  theme_bw()

# Area by rank and source
lc_comb %>%
  left_join(.,ranking) %>%
  group_by(source, rank) %>%
  summarize(area = sum(value, na.rm = T))


### SET PREFERRED SOURCE FOR CREATING LC MAP
lc_sel <- "rcmrd"


### COMPARE LC AND LU ADM0
# Combine
adm0_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 0) %>%
    summarise(value = sum(area, na.rm = T)) %>%
    mutate(rank = 0,
           type = "lu"),
  left_join(lc_comb, ranking) %>%
    filter(source == lc_sel) %>%
    group_by(rank) %>%
    #filter(rank %in% c(1,2)) %>%
    summarize(value = sum(value)) %>%
    mutate(type = "lc")) %>%
  mutate(rank = factor(rank,levels = c(4,3,2,1,0)))

ggplot(data = adm0_comp, aes(x = type, y = value, fill = rank)) +
  geom_col() +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw()


### COMPARE ADM2
# Combine
adm_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1) %>%
    group_by(adm) %>%
    summarise(value = sum(area, na.rm = T)) %>%
    mutate(rank = 0,
           type = "lu"),
  left_join(lc_comb, ranking) %>%
    filter(source == lc_sel) %>%
    left_join(adm_r)%>%
    group_by(adm, rank) %>%
    summarize(value = sum(value)) %>%
    mutate(type = "lc")) %>%
  mutate(rank = factor(rank,levels = c(4,3,2,1,0)))

ggplot(data = adm_comp, aes(x = type, y = value, fill = rank)) +
  geom_col() +
  facet_wrap(~adm) +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() 
  

### PREPARE FINAL LC MAP BY RANKING CELLS PER ADM
# Prepare travel time
tt <- stack(grid, tt)
tt <- as.data.frame(rasterToPoints(tt)) %>%
  dplyr::select(-x, -y)

# Prepare table with required lc based on lu statistics
# We distribute lu not covered by adm stat using relative lu area
area_not_adm <- sum(lu_adm_raw$area[lu_adm_raw$adm == iso3c_sel])-sum(lu_adm_raw$area[lu_adm_raw$adm != iso3c_sel])

target_lu <- lu_adm_raw %>%
  filter(adm_level == 1) %>%
  group_by(adm) %>%
  summarise(value = sum(area, na.rm = T)) %>%
  ungroup() %>%
  mutate(share = value/sum(value),
         target_lu = value + (share * area_not_adm)) %>%
  dplyr::select(adm, target_lu)

# Add ranking variables, sort on rank and tt and select up to target
# To make sure there is enough lc, we add slack
lc_slack <- 1.01

lc <- left_join(lc_comb, ranking) %>%
  filter(source == lc_sel) %>%
  left_join(tt) %>% 
  left_join(adm_r) %>%
  ungroup() %>%
  arrange(adm, rank, travel_time) %>%
  group_by(adm) %>%
  mutate(tot_area = cumsum(value)) %>%
  left_join(target_lu) %>%
  filter(tot_area <= target_lu*lc_slack) %>%
  ungroup() %>%
  dplyr::select(x, y, value, gridID)

lc_r <- dplyr::select(lc, -gridID)
lc_r <- rasterFromXYZ(lc_r)
crs(lc_r) <- crs(grid)

# Plot
# Plot ranking
ggplot() +
  geom_raster(data = lc, aes(x = x, y = y, fill = value)) +
  geom_path(data = adm, aes(x = long, y = lat, group = group), colour = "black") +
  geom_path(data = roads, aes(x = long, y = lat, group = group), colour = "grey50") +
  coord_equal() +
  theme_bw()


# x <- rasterFromXYZ(lc)
# x
# levelplot(x)
# levelplot(x, margin = F) +
#   layer(sp.polygons(adm, col = "grey")) +
#   layer(sp.polygons(roads, col = "black"))

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/maps/lc/lc_syn_30_sec_2000_", iso3c_sel, ".rds")))
writeRaster(lc_r, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/maps/lc/lc_syn_30_sec_2000_", iso3c_sel, ".tif")))
