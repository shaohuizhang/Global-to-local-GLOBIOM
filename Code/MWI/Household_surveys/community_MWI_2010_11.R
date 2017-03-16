# -------------------------------------
# community variables wave 1 2010-11
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
source("Code/get_dataPath.r")

# basic population info
comCC <- read_dta(file.path(dataPath, "/2010\\Data\\Community/COM_CC.dta")) %>%
  select(ea_id, popEA=com_cc02, HHEA=com_cc03)

# access to services variables
comCD <- read_dta(file.path(dataPath, "/2010\\Data\\Community/COM_CD.dta")) %>%
  select(ea_id, road=com_cd01, cost2small_town=com_cd11,
         cost2large_town=com_cd14, bank=com_cd66, micro_finance=com_cd68)

comCD$road <- toupper(as_factor(comCD$road))
comCD$bank <- toupper(as_factor(comCD$bank))
comCD$micro_finance <- toupper(as_factor(comCD$micro_finance))

# agriculture
comCF <- read_dta(file.path(dataPath, "/2010\\Data\\Community/COM_CF.dta")) %>%
  select(ea_id, plant_month=com_cf02, harv_month=com_cf03,
         ext_agent=com_cf07, dist2ext_agent=com_cf08a,
         unit = com_cf08b)

comCF$plant_month <- toupper(as_factor(comCF$plant_month))
comCF$harv_month <- toupper(as_factor(comCF$harv_month))
comCF$ext_agent <- toupper(as_factor(comCF$ext_agent))
comCF$unit <- as_factor(comCF$unit)
levels(comCF$unit) <- c(0.001, 1, 1.609, NaN)
comCF$unit <- as.numeric(as.character(comCF$unit))
comCF$dist2ext_agent <- comCF$unit * comCF$dist2ext_agent
comCF$unit <- NULL

# join all community variables
com_2010_11 <- full_join(comCC, comCD)
com_2010_11 <- full_join(com_2010_11, comCF)

# take out trash
rm(comCC, comCD, comCF, dataPath)