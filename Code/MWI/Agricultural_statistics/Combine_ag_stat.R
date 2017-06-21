#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse and combine agricultural statistics data from different sources
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen

# TO DO
# Add maps - copy from other script
# Add full list of crops and adm2 in final data file.
# remove yield and recalc in the end. 


### LOAD MAPPINGS
# Regional mapping
MWI2adm <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000)

crop_lvst <- read_excel(file.path(dataPath, "Data\\Mappings\\Mappings.xlsx"), sheet = "crop_lvst") %>%
  dplyr::select(short_name) %>%
  na.omit()


### LOAD DATA
# FAOSTAT
FAOSTAT <- read_csv(file.path(dataPath, "Data/MWI/processed/Agricultural_statistics/FAOSTAT_MWI.csv"))

# Agro-Maps
am <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/am_MWI.csv"))

# CropSTAT
cs <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/cs_MWI.csv"))

# Agricultural statistics
as <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/as_MWI.csv"))


### COMBINE ADM DATA AND ADD ID
# Filter out adm that are not relevant
# Select only values for 1999 and later as older info is not necessary
ag_stat <- bind_rows(am, as, cs, FAOSTAT) %>%
  mutate(id = paste(source, adm_level, sep = "_")) %>%
  filter(!adm %in% "AREA_UNDER_NATIONAL_ADMINISTRATION", year >= 1999) 


### COMPARE AREA AT COUNTRY LEVEL
# aggregate adm1 tot adm0
ag_stat_area_adm0 <- ag_stat %>%
  filter(variable %in% c("area"), year >= 1990) %>%
  group_by(year, adm_level, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T)) %>%
  mutate(id = paste(adm_level, source, sep = "_"))

# Figure
fig_area_adm0 <- ggplot(data = ag_stat_area_adm0, aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_adm0


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL
# Rank area of crops using FAOSTAT
tab_area_rank_FAOSTAT <- FAOSTAT %>%
  filter(year %in% c(2000, 2010), variable == "area") %>%
  group_by(year) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, share, year) %>%
  spread(year, share) %>%
  arrange(desc(`2010`))

# aggregate adm1 tot adm0
ag_stat_crop_adm0 <- ag_stat %>%
  filter(variable %in% c("area", "production"), year >= 1990) %>%
  group_by(id, year, adm_level, short_name, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T))

# Rank area of crops using all sources in base year
tab_area_rank_adm0 <- ag_stat_crop_adm0 %>%
  filter(variable == "area") %>%
  group_by(year, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year) %>%
  filter(year %in% c(2000)) %>%
  spread(id, share) %>%
  arrange(desc(`FAOSTAT_0`))


### COMPARE AREA AND PRODUCTION AT CROP AND COUNTRY LEVEL
# Area comparison
fig_area_crop_adm0 <- ggplot(data = filter(ag_stat_crop_adm0, variable == "area"), aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm0

# Production comparison
fig_prod_crop_adm0 <- ggplot(data = filter(ag_stat_crop_adm0, variable == "production"), aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Production comparison between FAOSTAT, am, as and cs",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_prod_crop_adm0


### COMPARE DATA AT ADM2 LEVEL
# Area data
ag_stat_area_adm2 <- ag_stat %>%
  filter(variable %in% c("area"), year >= 1990, adm_level == 2)

# Comparison of adm2 regions
fig_area_crop_adm2 <- ggplot(data = ag_stat_area_adm2, aes(x = year, y = value, colour = short_name, shape = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~adm, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm2

# Rank area of crops using all sources in base year
tab_area_rank_adm2 <- ag_stat_area_adm2 %>%
  group_by(year, adm, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year, adm) %>%
  filter(year %in% c(2000, 2007)) %>%
  spread(adm, share)

# Maps of adm2 with largest share of crop
# TO ADD


### CORRECT DATA
# We use FAOSTAT and am_2 data
ag_stat_upd <- ag_stat

### CORRECT FAOSTAT DATA
# Check if area data is complete
ggplot(data = filter(ag_stat_upd, variable == "area", id == "FAOSTAT_0"), aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Production comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  geom_vline(xintercept = c(1999, 2001), linetype = "dashed")

# Check if prod data is complete
ggplot(data = filter(ag_stat_upd, variable == "production", id == "FAOSTAT_0"), aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Production comparison between FAOSTAT, am, as and cs",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  geom_vline(xintercept = c(1999, 2001), linetype = "dashed")

xtabs(~variable + year + short_name, data = filter(ag_stat_upd, id == "FAOSTAT_0"))


# (1) soyb area, production and yield missing for 1999-2003
# (2) orts area and yield missing but production available
# (3) FAOSTAT does not present data on sweet potato production  

# (1) Fix soya. We assume area, production and yield have been the same as last year available for historical period.
soya_2003 <- ag_stat_upd %>%
  filter(id == "FAOSTAT_0", short_name == "soyb", year == 2003)
soya_fix <- soya_2003[rep(1:3,4),]
soya_fix$year <- c(rep(1999, 3), rep(2000,3), rep(2001,3), rep(2002,3))
soya_fix$source <- "updated"
ag_stat_upd <- bind_rows(ag_stat_upd, soya_fix)
rm(soya_fix, soya_2003)

# (2) Fix orts. We assume same yield as cassava and estimate area
orts_yield <- ag_stat_upd %>%
  filter(id == "FAOSTAT_0", short_name == "cass", variable == "yield") %>%
  mutate(short_name = "orts")

orts <- ag_stat_upd %>%
  filter(id == "FAOSTAT_0", short_name == "orts") %>%
  bind_rows(.,orts_yield) %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(area = production/yield) %>%
  gather(variable, value, -source, -id, -adm, -year, -adm_level, -short_name) %>%
  mutate(unit = dplyr::recode(variable, "production" = "tons", "area" = "ha", "yield"= "tons/ha"),
         source = "modified")

ag_stat_upd <- ag_stat_upd %>%
  filter(!(short_name == "orts" & id == "FAOSTAT_0")) %>%
  bind_rows(., orts)

rm(orts, orts_yield)  

# (3) Use shares from cs_1 to split FAOSTAT data
pota <- c("pota", "swpo")

# cs_1 shares shares
cs_1_pota <- ag_stat_upd %>%
  filter(id == "cs_1", short_name %in% pota, 
         variable %in% c("area", "production")) %>%
  group_by(variable, year, short_name) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(variable, year) %>%
  mutate(share = value/sum(value)) %>%
  dplyr::select(short_name, share, variable, year) 

# Split FAOSTAT
FAOSTAT_pota <- ag_stat_upd %>%
  filter(id == "FAOSTAT_0", short_name %in% c("pota"), variable %in% c("area", "production"))


# Function to split value using shares
split2_f <- function(df, crops, share_df, source){
  var <- df$variable
  number_crops <- length(crops)
  split_df <- df[rep(1, number_crops),]
  split_df$short_name <- crops
  impute_values <- share_df[share_df$variable == var,]
  impute_values $year <- NULL
  comb_df <- left_join(split_df, impute_values) %>%
    mutate(value = value*share,
           source = source) %>%
    dplyr::select(-share)
  return(comb_df)
}

# Calculate split values and add yield
FAOSTAT_pota_upd <- FAOSTAT_pota %>%
  group_by(adm, year, variable) %>%
  do(split2_f(., pota, cs_1_pota, "modified")) %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(yield = production/area) %>%
  gather(variable, value, -source, -id, -adm, -year, -adm_level, -short_name) %>%
  mutate(unit = dplyr::recode(variable, "production" = "tons", "area" = "ha", "yield"= "tons/ha"),
         source = "modified")


# Replace values
ag_stat_upd <- ag_stat_upd %>%
  filter(!(id == "FAOSTAT_0" & short_name %in% c("pota"))) %>%
  bind_rows(FAOSTAT_pota_upd)

# Clean up
rm(pota, FAOSTAT_pota_upd, cs_1_pota)


### CORRECT ADM DATA
# Check if data is complete
ggplot(data = filter(ag_stat_upd, variable == "area", id == "am_2"), aes(x = year, y = value, colour = short_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~adm, scales = "free") +
  labs(title = "Production comparison between FAOSTAT, am, as and cs",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  geom_vline(xintercept = c(1999, 2001), linetype = "dashed")

xtabs(~ variable + year + short_name, filter(ag_stat_upd, id == "am_2"))
xtabs(~ adm + year, filter(ag_stat_upd, id == "am_2"))

# Check if adm2 areas are missing
# No info on Nkhata Bay
am_2_adm2 <- unique(am$adm)
adm_missing <- MWI2adm$adm2_am[!MWI2adm$adm2_am %in% am_2_adm2]
adm_missing

# (1) Use data from am_2 for harvested area. 
# (2) For some adm2 production, area and yield is missing for 2001, in particular Chiradzulu, Likoma, Thylo. 
# (3) Comparison with available crop land shows that maize area in Blantyre is implausible high (more than total land area!)
# (4) Data for the adm2 Nkhata Bay is missing
# (5) All pulses seem to be categorises under opul.

# (2) use data from last available year as data Chiradzulu, Likoma, Thylo.
chiradzulu <- ag_stat_upd %>%
  filter(id == "am_2", adm == "CHIRADZULU", year == 2002)
chiradzulu_fix <- chiradzulu[rep(1:nrow(chiradzulu),3),]
chiradzulu_fix$year <- c(rep(1999:2001, each = nrow(chiradzulu)))
chiradzulu_fix$source <- "updated"
ag_stat_upd <- bind_rows(ag_stat_upd, chiradzulu_fix)
rm(chiradzulu_fix)

likoma <- ag_stat_upd %>%
  filter(id == "am_2", adm == "LIKOMA", year == 2004)
likoma_fix <- likoma[rep(1:nrow(likoma),5),]
likoma_fix$year <- c(rep(1999:2003, each = nrow(likoma)))
likoma_fix$source <- "updated"
ag_stat_upd <- bind_rows(ag_stat_upd, likoma_fix)
rm(likoma_fix)

thyolo <- ag_stat_upd %>%
  filter(id == "am_2", adm == "THYOLO", short_name %in% c("cass", "cott", "grou", "opul", "sorg"), year == 2002)
thyolo_fix <- thyolo[rep(1:nrow(thyolo),3),]
thyolo_fix$year <- c(rep(1999:2001, each = nrow(thyolo)))
thyolo_fix$source <- "updated"
ag_stat_upd <- bind_rows(ag_stat_upd, thyolo_fix)
rm(thyolo_fix)

# (3) We estimate maize as the share of all other crops in as_2 for the period 1999-2006
blantyre <- filter(ag_stat_upd, adm == "BLANTYRE", variable %in% c("area", "production"))
blantyre_maize_share <- filter(blantyre, id == "as_2") %>%
  group_by(id, year, variable) %>%
  mutate(share = value[short_name == "maiz"]/sum(value[short_name != "maize"])) %>%
  ungroup() %>%
  dplyr::select(share, variable) %>%
  unique()

# Set period
y <- unique(blantyre$year[blantyre$id == "am_2"])
period = length(y)

blantyre_fix <- filter(blantyre, id == "am_2", short_name != "maiz") %>%
  group_by(variable, adm, unit, adm_level, id) %>%
  summarize(value = sum(value, na.rm = T)/period) %>%
  left_join(blantyre_maize_share) %>%
  mutate(short_name = "maiz",
         value = share * value,
         source = "modified") %>%
  dplyr::select(-share)

# Copy values
blantyre_fix <- blantyre_fix %>%
  ungroup() %>%
  group_by(variable) %>%
  do(copy_val_f(., y))

# Replace values
ag_stat_upd <- ag_stat_upd %>%
  filter(!(adm == "BLANTYRE" & id == "am_2" & short_name == "maiz")) %>%
  bind_rows(blantyre_fix)

# Clean up
rm(blantyre, blantyre_fix, blantyre_maize_share, period)

# (4) We use information from as_2 to estimate Nkhata bay area and production
# We use shares of Nkhata bay in national total for each crop
# We only select the crops that are also covered by am_2
am_2_crops <- unique(am$short_name)

# We have to aggregate pulses into other pulses first
pulses <- c("bean", "chic", "cowp", "pige", "lent", "opul")
nkata_share <- bind_rows(
  filter(ag_stat_upd, id == "as_2", short_name %in% pulses) %>%
    group_by(variable, adm) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(short_name = "opul"),
  filter(ag_stat_upd, id == "as_2", !(short_name %in% pulses)) %>%
    dplyr::select(value, adm, variable, short_name))

# Calculate shares
nkata_share <- nkata_share %>%
  ungroup() %>%
  group_by(short_name, variable) %>%
  mutate(share = value/sum(value, na.rm = T)) %>%
  filter(adm == "NKHATA_BAY", short_name %in% am_2_crops) %>%
  dplyr::select(short_name, variable, share)

nkata_fix <- filter(ag_stat_upd, id == "am_2", variable != "yield") %>%
  ungroup() %>%
  group_by(year, short_name, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(nkata_share) %>%
  mutate(value = share*value,
         adm = "NKHATA_BAY",
         adm_level = 2,
         source = "modified",
         id = "am_2",
         value = ifelse(is.na(value), 0, value)) %>% # Set cotton NA values to 0 as production is 0 anyway.
  dplyr::select(-share)

# Add data
ag_stat_upd <- bind_rows(ag_stat_upd, nkata_fix)

# (5) We use shares from FAOSTAT pulses to split am_2 data
pulses <- c("bean", "chic", "cowp", "pige", "lent", "opul")

# FAOSTAT shares
FAOSTAT_pulses <- ag_stat_upd %>%
  filter(id == "FAOSTAT_0", short_name %in% pulses, 
         variable %in% c("area", "production")) %>%
  group_by(variable, year) %>%
  mutate(share = value/sum(value)) %>%
  dplyr::select(short_name, share, variable, year) 

# Split am2
am_2_pulses <- ag_stat_upd %>%
  filter(id == "am_2", short_name %in% c("opul"), variable %in% c("area", "production"))


# Function to split value using shares
split_f <- function(df, crops, share_df, source){
  var <- df$variable
  number_crops <- length(crops)
  split_df <- df[rep(1, number_crops),]
  split_df$short_name <- crops
  impute_values <- share_df[share_df$variable == var & share_df$year == df$year,]
  comb_df <- left_join(split_df, impute_values) %>%
    mutate(value = value*share,
           source = source) %>%
    dplyr::select(-share)
  return(comb_df)
}

# Calculate split values and add yield
am_2_pulses_upd <- am_2_pulses %>%
  group_by(adm, year, variable) %>%
  do(split_f(., pulses, FAOSTAT_pulses, "modified")) %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(yield = production/area) %>%
  gather(variable, value, -source, -id, -adm, -year, -adm_level, -short_name) %>%
  mutate(unit = dplyr::recode(variable, "production" = "tons", "area" = "ha", "yield"= "tons/ha"),
         source = "modified") %>%
  na.omit()

# Replace values
ag_stat_upd <- ag_stat_upd %>%
  filter(!(id == "am_2" & short_name %in% c("opul"))) %>%
  bind_rows(am_2_pulses_upd)

# Clean up
rm(pulses, FAOSTAT_pulses, am_2_pulses_upd)


#### PLOT UPDATED AG_STAT
# adm 2
ggplot(data = filter(ag_stat_upd, variable == "area", id == "am_2"), aes(x = year, y = value, colour = short_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~adm, scales = "free") +
  labs(title = "Production comparison between FAOSTAT, am, as and cs",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  geom_vline(xintercept = c(1999, 2001), linetype = "dashed")

xtabs(~ variable + year + short_name, filter(ag_stat_upd, id == "am_2"))
xtabs(~ adm + year, filter(ag_stat_upd, id == "am_2"))


# aggregate adm1 tot adm0
ag_stat_crop_adm0_upd <- ag_stat_upd %>%
  filter(variable %in% c("area", "production"), year >= 1990) %>%
  group_by(id, year, adm_level, short_name, unit, variable) %>%
  summarize( value = sum(value, na.rm = T))

fig_area_crop_adm0_upd <- ggplot(data = filter(ag_stat_crop_adm0_upd, variable == "area"), aes(x = year, y = value, colour = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm0_upd


### HARMONISE ADM TOTAL WITH NATIONAL TOTAL FROM FAOSTAT
# Create 2000 database
# Remove yields which are recalculated after averaging
# Remove source info as this might give problems if either producion or area is updated and sources differ
# Only focus on area for now

# Create FAO adm0 data: Calculate averages for 1999-2001
FAOSTAT_2000 <- ag_stat_upd %>%
  filter(id %in% c("FAOSTAT_0"), variable %in% c("area"), year %in% c(1999:2001)) %>%
  group_by(short_name, id, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  dplyr::select(adm, value, short_name, adm_level, variable)

# Create adm2 area data: Use 2001 data as data for 1999-2000 because much data is missing for 1999-2000 period
adm_2000 <-  ag_stat_upd %>%
  ungroup() %>%
  filter(id == "am_2", year %in% c(2001), variable == "area") %>%
  dplyr::select(-unit, -source) %>%
  group_by(short_name, variable) %>%
  mutate(adm0 = sum(value)) %>%
  left_join(transmute(FAOSTAT_2000, short_name, variable, FAOSTAT = value)) %>%
  mutate(value = value/adm0*FAOSTAT) %>%
  dplyr::select(adm, value, short_name, adm_level, variable) %>%
  ungroup()


# Combine and plot
ag_stat_2000 <- bind_rows(FAOSTAT_2000, adm_2000)


### ADD ZERO FOR CROPS THAT ARE NOT PRODUCED IN AN ADM2. 
# check coverage
xtabs(~ adm + short_name, data = filter(ag_stat_2000, adm_level == 2))

# Fill in zero
ag_stat_2000_adm2 <- filter(ag_stat_2000, adm_level == 2) %>%
  spread(short_name, value, fill = 0) %>%
  gather(short_name, value, -adm, -adm_level, -variable)

# Replace
ag_stat_2000 <- filter(ag_stat_2000, !(adm_level == 2)) %>%
  bind_rows(ag_stat_2000_adm2)


### COMPARE AGRICULTURAL STATISTICS WITH CROP COVER
# LOAD DATA
# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_2000_MWI.rds")) %>%
  mutate(type = "land_cover")

# COMPARE ADM0


# COMPARE ADM2
# Aggregate crop cover at adm 2 level
cc_adm2 <- crop_cover %>%
  group_by(adm2, type) %>%
  summarize(value = sum(area, na.rm = T)) %>%
  rename(adm = adm2)

# land use at adm 2 level
lu_adm2 <- ag_stat_2000 %>%
  filter(variable == "area", adm_level == 2) %>%
  mutate(type = "land_use") %>%
  group_by(adm, type) %>%
  summarize(value = sum(value, na.rm = T)) 

# Combine and plot
adm2 <- bind_rows(lu_adm2, cc_adm2)

ggplot(data = adm2, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# COMPARE ADM0
# Aggregate crop cover at adm 0 level
cc_adm0 <- crop_cover %>%
  group_by(adm0, type) %>%
  summarize(value = sum(area, na.rm = T))

# land use at adm 0 level
lu_adm0 <- ag_stat_2000 %>%
  filter(variable == "area" & adm_level == 0) %>%
  rename(adm0 = adm) %>%
  mutate(type = "land_use") %>%
  group_by(type) %>%
  summarize(value = sum(value, na.rm = T)) 

# Combine and plot
adm0 <- bind_rows(lu_adm0, cc_adm0)

ggplot(data = adm0, aes(x = adm0, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


### CORRECT DISCREPANCY LAND COVER AND LAND USE
# For some adm2 crop area is larger than land cover area from maps. 
# This is impossible unless, there is double cropping, fallowed land or data errors.
# As irrigated areas are not part of the model yet, we create a test dataset that simply shrinks the land use with a certain factor

# Calculate correction factor
adm2_corr <- adm2 %>%
  spread(type, value) %>%
  mutate(factor = land_use/land_cover) %>%
  filter(factor > 1) %>%
  dplyr::select(adm, factor)

# Correct area
adm2_maiz_corr <- ag_stat_2000 %>%
  filter(adm %in% adm2_corr$adm) %>%
  left_join(adm2_corr) %>%
  mutate(value = value/factor) %>%
  dplyr::select(-factor)

# replace values
ag_stat_2000[ag_stat_2000$adm %in% adm2_corr$adm, ] <- adm2_maiz_corr


# Save
write_csv(ag_stat_2000, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))
