#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to process aquastat irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("countrycode")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD DATA
# Aquastat raw
aquastat_raw <- read_excel(file.path(dataPath, "Data/Global/Aquastat/20171113_aquastat_irrigation.xlsx"), sheet = "data")

# Crop mapping
aquastat2crop_lvst <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "aquastat2crop_lvst")

### PROCESS DATA
# Clean up database
aquastat <- aquastat_raw %>%
  mutate(iso3c = countrycode(`Area Id`, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  transmute(iso3c, variable = `Variable Name`, variable_code = `Variable Id`, year = Year, ir_area = Value)

# Create irrigated area df
# Note that "Total harvested irrigated crop area (full control irrigation)" (4379) is only presented if all crops are included
ir_area <- aquastat %>%
  dplyr::filter(grepl("Harvested irrigated temporary crop area", variable)|
                grepl("Harvested irrigated permanent crop area", variable)|
                variable_code %in% c(4379, 4313)) %>%
  separate(variable, c("variable", "aquastat_crop"), sep = ":") %>%
  mutate(aquastat_crop = trimws(aquastat_crop),
         aquastat_crop = ifelse(is.na(aquastat_crop), "Total", aquastat_crop),
         aquastat_crop = ifelse(aquastat_crop == "total", "Total", aquastat_crop))

# Map to crop_lvst crops
# NB: Depending on country select tropical (trof) or temperate fruits for "Other fruits" category if available
# NB: fodder and Grass and fodder are not mapped

ir_area <- ir_area %>%
  left_join(aquastat2crop_lvst) %>%
  group_by(iso3c, variable, year, short_name) %>%
  summarize(ir_area = sum(ir_area, na.rm = T))

# Save
write_csv(ir_area, file.path(dataPath, "Data/Global/AQUASTAT/Irrigated_area_by_crop.csv"))
         
 

         