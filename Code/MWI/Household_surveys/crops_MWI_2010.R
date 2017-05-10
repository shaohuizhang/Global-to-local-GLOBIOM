#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to prepare crop level dataframe
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
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\MWI\\Raw\\Household_survey\\2010\\IHS3"

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

# load crop codes
crop_list <- read_csv(file.path(dataPath, "Conversion/MWI_crop_list_2010.csv"))

# crop output
crops2010 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_G.dta")) %>%
  select(case_id, ea_id, plotnum=ag_g0b, crop_code=ag_g0d,
         crop_stand=ag_g01, crop_share=ag_g03, harv_start = ag_g12a,
         harv_end = ag_g12b, crop_qty_harv=ag_g13a,
         unit=ag_g13b, condition=ag_g13c)

# change one_crop to crop stand
crops2010$crop_stand <- ifelse(crops2010$crop_stand %in% 1, 1,
                        ifelse(crops2010$crop_stand %in% 2, 0, NA))
crops2010$crop_share <- as_factor(crops2010$crop_share)
crops2010$harv_start <- as_factor(crops2010$harv_start)
crops2010$harv_end <- as_factor(crops2010$harv_end)
crops2010$crop_code <- as.integer(crops2010$crop_code)
crops2010$unit <- as.integer(crops2010$unit)
crops2010$condition <- as.integer(crops2010$condition)
crops2010$crop_code <- as.integer(crops2010$crop_code)

# Link cropcodes
crops2010 <- left_join(crops2010, crop_list)

#' crop quantities are recorded in non-standard units.
#' The world bank provided (upon request) a file with
#' the correct conversion units per region, crop, unit,
#' and condition

# get region variable
region <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = hh_a01)
region$region <- with(region,
                             ifelse(district < 200, 1,
                                    ifelse(district >=200 & district < 300, 2,
                                           3)))
region$district <- NULL
region$region <- as.integer(region$region)

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "Conversion/IHS.Agricultural.Conversion.Factor.Database.dta"))
qty2kg$crop_code <- as.integer(qty2kg$crop_code)
qty2kg$unit <- as.integer(qty2kg$unit)
qty2kg$condition <- as.integer(qty2kg$condition)
qty2kg$region <- as.integer(qty2kg$region)
qty2kg$flag <- NULL

# join region variable to the output variables
# and then join with the conversion factors
crops2010 <- left_join(crops2010, region)

# attributes in the qty2kg conversion file
# prevent joining attributes. Strip attributes
# first
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}
qty2kg <- stripAttributes(qty2kg)

# now join works with no errors
crops2010 <- left_join(crops2010, qty2kg)

# multiply the recorded quantity by conversion
# to kilograms
crops2010$crop_qty_harv2 <- crops2010$crop_qty_harv * crops2010$conversion
crops2010$unit <- crops2010$shell_unshelled <- crops2010$conversion <-
  crops2010$condition <- NULL

# crop production from the rainy season of 2010_11
# in order to get unit prices of each crop.
# These need to be matched with region and then
# converted as above

crop_unit_priceRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(case_id, ea_id, crop_code = ag_i0b,
         qty_harv = ag_i02a, unit = ag_i02b,
         condition = ag_i02c, crop_value = ag_i03) %>% unique()
crop_unit_priceRS$crop_code <- as.integer(crop_unit_priceRS$crop_code)
crop_unit_priceRS$unit <- as.integer(crop_unit_priceRS$unit)
crop_unit_priceRS$condition <- as.integer(crop_unit_priceRS$condition)

# Join with region and then conversion factor
crop_unit_priceRS <- left_join(crop_unit_priceRS, region)
crop_unit_priceRS <- left_join(crop_unit_priceRS, qty2kg)

# make conversion and calculate the crop prices
crop_unit_priceRS$qty_harv <- crop_unit_priceRS$qty_harv * crop_unit_priceRS$conversion
crop_unit_priceRS$crop_price <- crop_unit_priceRS$crop_value/crop_unit_priceRS$qty_harv
crop_unit_priceRS <- select(crop_unit_priceRS, case_id, ea_id, crop_code, crop_value, qty_harv, crop_price)

# join prices with output
crops2010_2 <- left_join(crops2010,
                          crop_unit_priceRS)

# if someone either did not have any output or
# had 0 output remove them from the data frame
crops2010 <- crops2010[! is.na(crops2010$crop_qty_harv) & !crops2010$crop_qty_harv %in% 0, ]

# At this point, in theory, we don't need the units
# anymore as everything is in kg, and we don't care
# about whether the crop was shelled or unshelled as
# this should also be accounted for in the units

crops2010$crop_qty_harv_unit <- crops2010$crop_qty_harvSU <-
  crops2010$region <- NULL



# take out the trash
rm(list=c("dataPath", "region", "qty2kg", "crops2010_2", "crop_unit_priceRS"))
