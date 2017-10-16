#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to depict yield scenarios
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load()


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


### LOAD FAOSTAT DATA
faostat_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/FAOSTAT_", iso3c_sel, ".csv")))


### CREATE GRAPH
# Filter crops
faostat <- faostat_raw %>% 
  filter(short_name %in% c("maiz", "grou", "cott"),
         variable == "yield")

# Plot
ggplot(data = faostat, aes(x = year, y = value)) +
  geom_line(aes(colour = short_name, linetype = short_name), size = 1.5) +
  scale_x_continuous(limits = c(1960, 2050), breaks = seq(1960, 2050, 10)) +
  scale_colour_discrete(breaks=c("cott", "grou", "maiz"),
                      labels=c("Cotton", "Groundnuts", "Maize")) +
  scale_linetype_discrete(breaks=c("cott", "grou", "maiz"),
                        labels=c("Cotton", "Groundnuts", "Maize")) +
  theme_bw() +
  labs(x = "", y = "tons/ha", colour = "", linetype = "") +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  theme(legend.position = c(.15,.8)) +
  theme(legend.background = element_rect(colour = "black")) +
  theme(panel.grid.minor = element_blank())

