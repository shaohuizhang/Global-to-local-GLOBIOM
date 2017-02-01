# -------------------------------------
#' Income variables for Malawi wave 1
#' (2010-11). Information on income can be
#' found in the following sections of the
#' questionnaire:
#' 
#' off-farm income (household questionnaire):
#'      1. Module E for off-farm income vars
#'      2. Module N household enterprises
#'      3. Module P other income
#'      
#' on-farm income (agriculture questionnaire):
#'      1. Sections 2a and 2b question 16 for rent income
#'      2. Section 5a for crop production in first cropping season (2009)
#'      3. Section 5b for crop production in second cropping season (2009)
#'      4. Sections 6A, 6B and 6C for livestock sales
#'      5. Section 8 for livestock products
#'      6. Section 9 for fish sold (not used)
#'
#' The output of this file is a dataframe
#' holding all the income variables for each
#' household 
#' 
#' local currency: Malawian Kwacha (mk)
#' 
#' Maize crop codes:
#' 1: maize local
#' 2: maize composite
#' 3: maize hybrid
#' 4: maize hybrid recycled
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2010\\Data"
}

# -------------------------------------
#' off-farm income
#' Although respondents were asked whether 
#' they had a primary or secondary job 
#' The period in which they were paid was
#' often in days. Unfortunately, the 
#' number of days worked was not recorded
#' and reliable income estimates can
#' therefore not be calculated.
#' 
#' Ganyu labour, short term rural labour,
#' is recorded in a way that makes it
#' possible to calculate and will be
#' recorded as ganyu income.  
# -------------------------------------

# select all income variables
off_farm_income <- read_dta(file.path(dataPath, "Household/HH_MOD_E.dta")) %>%
  select(HHID, case_id, PID,
         mj_industry=hh_e20b, mj_employer=hh_e21,
         mj_months=hh_e22, mj_weekspm=hh_e23,
         mj_hourspw=hh_e24,mj_wage=hh_e25,
         mj_pay_units=hh_e26a, mj_pay_period=hh_e26b, # time units and pay period 
         mj_grat_wage=hh_e27,
         mj_grat_pay_units=hh_e28a, mj_grat_pay_period=hh_e28b, # time units and pay period
         sj_industry=hh_e34b, sj_employer=hh_e35,
         sj_months=hh_e36, sj_weekspm=hh_e37,
         sj_hourspw=hh_e38, sj_wage=hh_e39,
         sj_pay_units=hh_e40a, sj_pay_period=hh_e40b, # time units and pay period 
         sj_grat_wage=hh_e41,
         sj_grat_pay_units=hh_e42a, sj_grat_pay_period=hh_e42b, # time units and pay period
         ganyu=hh_e55, ganyu_months=hh_e56, ganyu_weekspm=hh_e57,
         ganyu_dayspw=hh_e58, ganyu_wagepd=hh_e59)

# as shown in the tables below, the most common
# pay period is "day". We look only an Ganyu labour
table(as_factor(off_farm_income$mj_pay_period)) # mostly day
table(as_factor(off_farm_income$mj_grat_pay_period)) # mostly day
table(as_factor(off_farm_income$sj_pay_period)) # mostly day
table(as_factor(off_farm_income$sj_grat_pay_period)) # mostly day

off_farm_income <- select(off_farm_income, HHID, case_id, PID,
                          ganyu, ganyu_months, ganyu_weekspm,
                          ganyu_dayspw, ganyu_wagepd) %>%
  transmute(HHID, case_id, PID, ganyu,
            ganyu_income = ganyu_months * ganyu_weekspm * ganyu_dayspw * ganyu_wagepd)

off_farm_income$ganyu <- ifelse(off_farm_income$ganyu %in% 1, 1,
                          ifelse(off_farm_income$ganyu %in% 2, 0, NA))

off_farm_income_hh <- group_by(off_farm_income, HHID, case_id) %>%
  summarise(ganyu_income_hh=sum(ganyu_income, na.rm=TRUE))
rm(off_farm_income)

# -------------------------------------
# Module P: Other income
# -------------------------------------

off_farm_income_other <- read_dta(file.path(dataPath, "Household/HH_MOD_P.dta")) %>%
  select(HHID, case_id, code=hh_p0a, income_other=hh_p02)

# summarise to the household level
off_farm_income_other <- group_by(off_farm_income_other, HHID, case_id) %>%
  summarise(income_other=sum(income_other, na.rm=TRUE))

# -------------------------------------
# Module N: household enterprise activities
# Ignore for now for speed - most values
# are at the month level, not clear how to
# scale up to the year level
# -------------------------------------


# -------------------------------------
#' on-farm income
# -------------------------------------

# crop production from the rainy season of 2010_11
cropRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(HHID, case_id, crop_code = ag_i0b,
         qty_harv = ag_i02a, unit = ag_i02b,
         condition = ag_i02c, crop_value = ag_i03)

# crop production from the dry season of 2010_11
cropDS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_O.dta")) %>%
  select(HHID, case_id, crop_code = ag_o0b,
         qty_harv = ag_o02a, unit = ag_o02b,
         condition = ag_o02c, crop_value = ag_o03)

# join both together
crop <- rbind(cropRS, cropDS)

# calculate the full value of crops
# per household

on_farm_income_crop <- group_by(crop, HHID, case_id) %>%
  summarise(crop_value_hh=sum(crop_value, na.rm=TRUE))
rm(crop, cropRS, cropDS)

# tree crops
tree <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_Q.dta")) %>%
  select(HHID, case_id, tree_value=ag_q03)

# summarise to the household level
on_farm_income_tree <- group_by(tree, HHID, case_id) %>%
  summarise(tree_value_hh=sum(tree_value, na.rm=TRUE))

# rent on land rainy season
rentRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select(HHID, case_id, plot_id=ag_d00, rent_cash_rec = ag_d19a,
         rent_in_kind_rec = ag_d19b, rent_cash_later=ag_d19c,
         rent_in_kind_later=ag_d19d)

# rent on land dry season
rentDS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, plot_id=ag_k0a, rent_cash_rec = ag_k20a,
         rent_in_kind_rec = ag_k20b, rent_cash_later=ag_k20c,
         rent_in_kind_later=ag_k20d)

# join the two types of rent together
rent <- rbind(rentRS, rentDS)

# calcualte an overall rent 
rent$rent <- with(rent,
                  rowSums(cbind(rent_cash_rec,rent_in_kind_rec,rent_cash_later, rent_in_kind_later),
                          na.rm=TRUE))
miss <- with(rent,
             is.na(rent_cash_rec) & is.na(rent_in_kind_rec) & is.na(rent_cash_later) & is.na(rent_in_kind_later))
rent$rent[miss] <- NA; rm(miss)

# summarise to the household level
on_farm_income_rent <- group_by(rent, HHID, case_id) %>%
  summarise(rent_value_hh=sum(rent, na.rm=TRUE))
rm(rent)


# -------------------------------------
# Livestock
# Respondents were asked if they had sold
# livestock. 
# respondents are asked if they slaughtered
# animals for sale, but the income derived
# is not recorded.
# -------------------------------------

# lvstock sales
lvstock_sales <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_R1.dta")) %>%
  select(HHID, case_id, lvstock_sales_value=ag_r17)

# summarise at the household level
on_farm_income_lvstock_sales <- group_by(lvstock_sales, HHID, case_id) %>%
  summarise(lvstock_sales_value_hh=sum(lvstock_sales_value, na.rm=TRUE))

# -------------------------------------
# Livestock products sold.
# -------------------------------------

lvstck_products <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_S.dta")) %>%
  select(HHID, case_id, lvstck_prod_value=ag_s06)

# summarise at the household level
on_farm_income_lvstock_products <- group_by(lvstck_products, HHID, case_id) %>%
  summarise(lvstock_prod_value_hh=sum(lvstck_prod_value, na.rm=TRUE))


# -------------------------------------
# Total household income Malawi 2013
# -------------------------------------

income_2010_11 <- full_join(off_farm_income_hh, off_farm_income_other)
income_2010_11 <- full_join(income_2010_11, on_farm_income_crop)
income_2010_11 <- full_join(income_2010_11, on_farm_income_tree)
income_2010_11 <- full_join(income_2010_11, on_farm_income_lvstock_sales)
income_2010_11 <- full_join(income_2010_11, on_farm_income_lvstock_products)
income_2010_11 <- full_join(income_2010_11, on_farm_income_rent)

# calculate a total income variable
income_2010_11$income <- with(income_2010_11,
                           rowSums(cbind(ganyu_income_hh, income_other,
                                         crop_value_hh, tree_value_hh,
                                         lvstock_sales_value_hh,
                                         lvstock_prod_value_hh,
                                         rent_value_hh),
                                   na.rm=TRUE))

# take out trash
rm(dataPath, lvstck_products, lvstock_sales,
   off_farm_income_hh, off_farm_income_other,
   on_farm_income_crop, on_farm_income_tree,
   on_farm_income_lvstock_sales,
   on_farm_income_lvstock_products,
   on_farm_income_rent,
   rentDS, rentRS,tree)
