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

# crop output
crops2010 <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Agriculture/AG_MOD_G.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_g0b, crop_code=as_factor(ag_g0d),
         crop_stand=as_factor(ag_g01), crop_share=ag_g03, harv_start = as_factor(ag_g12a),
         harv_end = as_factor(ag_g12b), crop_qty_harv=ag_g13a,
         unit=as_factor(ag_g13b), condition=as_factor(ag_g13c))

# Appear to be duplicates in raw data => remove
dupl <-  group_by_all(crops2010) %>% 
  filter(n()>1)
rm(dupl)

crops2010 <- unique(crops2010)


#' crop quantities are recorded in non-standard units.
#' The world bank provided (upon request) a file with
#' the correct conversion units per region, crop, unit,
#' and condition

# get region variable
region <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = hh_a01)
region$region <- with(region,
                             ifelse(district < 200, 1,
                                    ifelse(district >=200 & district < 300, 2,
                                           3)))
region$district <- NULL
region$region <- as.integer(region$region)

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Conversion/IHS.Agricultural.Conversion.Factor.Database.dta")) %>%
  transmute(crop_code = as_factor(crop_code), unit = toupper(as_factor(unit)), condition = toupper(as_factor(condition)), region = as.integer(region), 
            shell_unshelled, conversion)

# join region variable to the output variables
# and then join with the conversion factors
crops2010 <- left_join(crops2010, region) %>%
  left_join(., qty2kg)

# multiply the recorded quantity by conversion to kilograms
crops2010 <- crops2010 %>%
  mutate(crop_qty_harv = crop_qty_harv * crops2010$conversion) %>%
  dplyr::select(-unit, -shell_unshelled, -conversion, -region, -condition)


# if someone either did not have any output or
# had 0 output remove them from the data frame
crops2010 <- crops2010[! is.na(crops2010$crop_qty_harv) & !crops2010$crop_qty_harv %in% 0, ]

# take out the trash
rm(list=c("dataPath", "region", "qty2kg"))