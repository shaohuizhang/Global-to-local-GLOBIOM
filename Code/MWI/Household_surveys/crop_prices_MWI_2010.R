#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to prepare crop level dataframe
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "haven")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")



### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### OUTPUT
# -------------------------------------
# Output MWI 2010_11 (wave 1)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Conversion/IHS.Agricultural.Conversion.Factor.Database.dta")) %>%
  transmute(crop_code = as_factor(crop_code), unit = toupper(as_factor(unit)), condition = toupper(as_factor(condition)), region = as.integer(region), 
            shell_unshelled, conversion)


# get region variable
region <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = hh_a01)
region$region <- with(region,
                      ifelse(district < 200, 1,
                             ifelse(district >=200 & district < 300, 2,
                                    3)))
region$district <- NULL
region$region <- as.integer(region$region)

# crop production from the rainy season of 2010_11
# in order to get unit prices of each crop.
# These need to be matched with region and then
# converted as above
crop_unit_prices2010 <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Agriculture/AG_MOD_I.dta")) %>%
  transmute(case_id, ea_id, crop_code = as_factor(ag_i0b),
         qty_harv = ag_i02a, unit = as_factor(ag_i02b),
         condition = as_factor(ag_i02c), crop_value = ag_i03) %>% unique()

# Join with region and then conversion factor
crop_unit_prices2010 <- left_join(crop_unit_prices2010, region)
crop_unit_prices2010 <- left_join(crop_unit_prices2010, qty2kg)


# make conversion select relevant variables and filter our observations with missing information
crop_unit_prices2010 <- crop_unit_prices2010 %>%
  mutate(qty_harv  = qty_harv * conversion) %>%
  select(-conversion, -unit, - shell_unshelled, -condition) %>%
  filter(!is.nan(qty_harv), !is.nan(crop_value))

# Aggregate information over same crop_codes but with different units and calculate prices
crop_unit_prices2010 <- crop_unit_prices2010 %>%
  group_by(case_id, ea_id, crop_code) %>%
  summarize(qty_harv = sum(qty_harv, na.rm = T),
            crop_value = sum(crop_value, na.rm = T),
            crop_price = crop_value/qty_harv) %>%
  ungroup()

# take out the trash
rm(list=c("dataPath", "region", "qty2kg"))
