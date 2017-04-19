#'========================================================================================================================================
#' Project:  ISWEL
#' Subject:  Code to process CEEPA and extract relevant data for target countries
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
p_load("countrycode", "haven")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM" 

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### FUNCTIONS
# Function to strip attributes and add classes
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}

### LOAD CROP CODES
crop_code_list <- read_excel(file.path(dataPath, "Data/ZWE/Raw/Household_surveys/CEEPA/CEEPA_crop_codes.xlsx"))

### DOWNLOAD DATA
CEEPA_raw <- read_dta(file.path(dataPath, "Data/ZWE/Raw/Household_surveys/CEEPA/CEEPASurvey.dta")) %>%
  mutate(adm0 = as_factor(adm0)) %>%
  filter(adm0 %in% c("zimbabwe"))

# LOCATION VARIABLES
location <- CEEPA_raw %>% 
  transmute(country = adm0,
         adm1 = as_factor(adm1),
         adm2 = as_factor(adm2),
         iso3c = countrycode(adm0, "country.name", "iso3c"),
         hhcode) %>%
  stripAttributes() %>%
  dplyr::select(-country)


### Section 4_1
# Check number of plots => max 6 and only 7 in lvs
#names(CEEPA_raw)[grep("p7" , names(CEEPA_raw))]

### INFORMATION AT PLOT, CROP AND SEASON LEVEL
### section 4_1
sect4_1 <- dplyr::select(CEEPA_raw, hhcode, s1p1c1:s1p3c6sval) %>%
  gather(variable, value, -hhcode) %>%
  separate(variable, c("season", "plotid", "crop_number", "indicator"), sep = c(2, 4, 6), remove = F) %>%
  mutate(indicator = ifelse(indicator == "", "crop_code", indicator)) %>%
  dplyr::select(-variable) %>%
  spread(indicator, value) %>%
  mutate_at(vars(area:sval), funs(replace(., is.nan(.), NA))) %>%
  filter(!is.na(crop_code)) %>%
  dplyr::select(-crop_number) %>% # replace all nan by NA
  left_join(., crop_code_list)

### YIELD AT HH AND CROP LEVEL
### Section 4_12
sect4_12 <- dplyr::select(CEEPA_raw, hhcode, pc1:pc5, nyieldc1:nyieldc5) %>%
  gather(variable, value, -hhcode) %>%
  separate(variable, c("var", "crop_number"), sep = c(-2), remove = T) %>%
  spread(var, value) %>%
  rename(crop_code = pc, yld = nyieldc) %>%
  left_join(., crop_code_list) %>%
  dplyr::filter(!is.na(crop_name))


### INPUTS AT PLOT AND SEASON LEVEL
### Section 4_13
sect4_13 <- dplyr::select(CEEPA_raw, hhcode, s1p1fert:s3p2wat5) %>%
  gather(variable, value, -hhcode) %>%
  separate(variable, c("season", "plotid", "indicator"), sep = c(2, 4), remove = T) %>%
  spread(indicator, value) %>%
  mutate_at(vars(fert:wat5), funs(replace(., is.nan(.), NA))) %>%
  dplyr::filter(season != "s3") # No information for s3

### PESTICIDE AND FERTILIZER COSTS
### Section 4_14 and 4_15
# Seems to be complete missing for Zimbabwe
sect4_14_15 <- dplyr::select(CEEPA_raw, hhcode, costkgfert, costkgpest)


### AGGREGATE AT ADM2 LEVEL
# yld
yld_ag <- left_join(location, sect4_12) %>%
  na.omit() %>%
  group_by(adm1, adm2, crop_name) %>%
  summarize(yld = mean(yld, na.rm = T)) %>%
  ungroup()

saveRDS(yld_ag, file.path(dataPath, "Data/ZWE/Processed/Household_surveys/yld_ag_ZWE.rds"))

no_inf <- sect4_13 %>%
  filter(rowSums(mutate_each(.[,c(4:14)], funs(is.na(.)))) != length(c(4:14)))
