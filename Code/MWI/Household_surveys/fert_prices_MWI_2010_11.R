# -------------------------------------
# fert prices MWI wave 1
# local currency Malawi MK
# There are two sections that
# deal with the inputs. Sections E 
# and F
# check out the agro questionnaire part of
# the MWI 2010-11 questionnaire
# for now focus on the rainy season.
# -------------------------------------

# set working directory
source("Code/get_dataPath.r")

# load packages
library(haven)
library(dplyr)

# -------------------------------------
# commercial fertilizer - there is also
# vouchers. But this is in a seperate file
# so the commercial fertilizer is the
# price that the farmer faces
# -------------------------------------

fert_prices_1 <- read_dta(file.path(dataPath, "/2010\\Data\\Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))),
            qty=ag_f16a, qty_unit = as_factor(ag_f16b), valu=ag_f19)
levels(fert_prices_1$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99, NaN)
fert_prices_1$unit2kg <- as.numeric(as.character(fert_prices_1$qty_unit)); fert_prices_1$qty_unit <- NULL
fert_prices_1$unit2kg <- ifelse(fert_prices_1$unit2kg > 90, NA, fert_prices_1$unit2kg)
fert_prices_1$qty <- fert_prices_1$qty * fert_prices_1$unit2kg
fert_prices_1$unit2kg <- NULL


fert_prices_2 <- read_dta(file.path(dataPath, "/2010\\Data\\Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))),
            qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29)
levels(fert_prices_2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99, NaN)
fert_prices_2$unit2kg <- as.numeric(as.character(fert_prices_2$qty_unit)); fert_prices_2$qty_unit <- NULL
fert_prices_2$unit2kg <- ifelse(fert_prices_2$unit2kg > 90, NA, fert_prices_2$unit2kg)
fert_prices_2$qty <- fert_prices_2$qty * fert_prices_2$unit2kg
fert_prices_2$unit2kg <- NULL

# We want the nitrogen price
# Combine both surveyyears, remove composite manure and other, and rename NPK
# there seems to be a lot of other chemicals in the
# MWI survey which are not in our conversion file.
# These need to be sorted into something we can work with

fert_prices_2010_11 <- bind_rows(fert_prices_1, fert_prices_2)
fert_prices_2010_11$typ <- ifelse(fert_prices_2010_11$typ %in% c("23:21:0+4", "23:21:0+4S", "23:21:0+4S/CHITOWE"),
                   "NPK (MWI)", fert_prices_2010_11$typ)
fert_prices_2010_11$typ <- ifelse(fert_prices_2010_11$typ %in% c("D.COMPOUND", "D COMPOUND"), "D compound", fert_prices_2010_11$typ)
fert_prices_2010_11 <- filter(fert_prices_2010_11, typ %in% c("D COMPOUND", "DAP", "CAN",
                                "UREA", "NPK (MWI)"))

# provide a nitrogen component value for npk and urea
conv <- read.csv(file.path(dataPath, "/../Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100)

# join the fertilizer information with the conversion
fert_prices_2010_11 <- left_join(fert_prices_2010_11, conv) %>%
  mutate(Vfert=valu/qty,
         Qn=qty*n,
         Qp=qty*p,
         price = Vfert/n)

# need to make a weighted price somehow to get prices at
# the household level
fert_prices_2010_11 <- group_by(fert_prices_2010_11, HHID, case_id) %>%
  summarise(WPn = sum((Qn/sum(Qn, na.rm=TRUE)) * price, na.rm=TRUE))

# take out trash
rm(conv, dataPath, fert_prices_1, fert_prices_2)  