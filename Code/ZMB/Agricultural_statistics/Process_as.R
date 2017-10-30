#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process agricultural statistics
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
#p_load("")


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


### OBTAIN ADM AND CROP_LVST LIST
# Load data
as_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Agricultural_statistics/Other/National_statistics/ag_statistics_1987_2014.xlsx"))) %>%
  dplyr::select(-Regions, -`Regions RegionId`, -`Regions TERRID`, -`Regions TOPO_ID`, -`Crop forecast`, -`Crop forecast Name`,
                -`Crop forecast`, -`Crop forecast DESCRIPTION`, -Units, -Scale) %>%
  rename(adm1_as = `Regions Name`, description = `Crop forecast FULLNAME`) %>%
  gather(year, value, -adm1_as, -description) %>%
  mutate(year = as.integer(year),
         adm1_as = toupper(adm1_as))
  
# Recode crops
as_raw$short_name <- NA
as_raw$short_name <- ifelse(grepl("Maize", as_raw$description), "maiz", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Sorghum", as_raw$description), "sorg", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Rice", as_raw$description), "rice", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Millet", as_raw$description), "mill", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Sun Flower", as_raw$description), "sunf", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Groundnuts", as_raw$description), "grou", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Soya Beans", as_raw$description), "soyb", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Wheat", as_raw$description), "whea", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Sweet Potatoes", as_raw$description), "swpo", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Cassava", as_raw$description), "cass", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Sugar Cane", as_raw$description), "sugc", as_raw$short_name)
as_raw$short_name <- ifelse(grepl("Mixed Beans", as_raw$description), "bean", as_raw$short_name)
# Additional line for cassava production that not includes cassava in description
as_raw$short_name <- ifelse(grepl("Crop Forecast survey- production Estimates", as_raw$description), "cass", as_raw$short_name)

# Recode variable
as_raw$variable <- NA
as_raw$variable <- ifelse(grepl("Area", as_raw$description, ignore.case=TRUE), "area", as_raw$variable)
as_raw$variable <- ifelse(grepl("Production", as_raw$description, ignore.case=TRUE), "production", as_raw$variable)
as_raw$variable <- ifelse(grepl("Prodution", as_raw$description, ignore.case=TRUE), "production", as_raw$variable)
as_raw$variable <- ifelse(grepl("Yield", as_raw$description, ignore.case=TRUE), "yield", as_raw$variable)
as_raw$variable <- ifelse(grepl("Yied", as_raw$description, ignore.case=TRUE), "yield", as_raw$variable)
as_raw$variable <- ifelse(grepl("Sales", as_raw$description, ignore.case=TRUE), "sales", as_raw$variable)
as_raw$variable <- ifelse(grepl("Milled", as_raw$description, ignore.case=TRUE), "milled", as_raw$variable)

# Add unit
as_raw$unit <- NA
as_raw$unit <- ifelse(as_raw$variable == "area", "ha", as_raw$unit)
as_raw$unit <- ifelse(as_raw$variable == "production", "tons", as_raw$unit)
as_raw$unit <- ifelse(as_raw$variable == "yield", "tons/ha", as_raw$unit)

# Clean up
as_raw <- as_raw %>%
  dplyr::select(adm1_as, year, short_name, variable, value, unit) %>%
  na.omit %>%
  filter(variable %in% c("yield", "production", "area"),
         adm1_as != "ZAMBIA")

# Save adm1 list
as_adm1_list <- as_raw %>%
  dplyr::select(adm1_as) %>%
  unique %>%
  arrange(adm1_as)
summary(as_adm1_list)

write_csv(as_adm1_list, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/as_", iso3c_sel, "_adm1_list.csv")))

# Save crop_lvst list
as_crop_lvst_list <- as_raw %>%
  dplyr::select(short_name) %>%
  unique %>%
  arrange(short_name)
summary(as_crop_lvst_list)

write_csv(as_crop_lvst_list, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/as_", iso3c_sel, "_crop_lvst_list.csv")))


### PROCESS
# Review and copy the as_crop_lvst_list and adm_list to the Mappings_ZMB.xlsx file before!
# We already recoded crops to short_name and added units

# Read adm1 mappping
adm1_map <- read_excel(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed/Mappings/Mappings_", iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_as) %>%
  na.omit %>%
  unique()

# Process data
as <- as_raw %>% 
  left_join(adm1_map) %>%
  group_by(year, adm1, short_name, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 1,
         source = "as") %>%
  rename(adm = adm1) %>%
  na.omit() # remove unmatched adm zones
summary(as)

# save file
write_csv(as, file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\agricultural_statistics\\as_", iso3c_sel, ".csv")))
