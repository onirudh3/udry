# Econ 590 PS2
# Anirudh Ravishankar
# December 12, 2025
# Data sourced from https://microdata.worldbank.org/index.php/catalog/3795

# Libraries
library(dplyr) # select(), %>%
library(fixest) # feols()

# WD
setwd("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/UGA_2018_UNPS_v02_M_CSV")


# Data --------------------------------------------------------------------

# AGSEC2A: Land parcels and plot characteristics
# hhid: household ID
# parcelID: plot ID
da <- read.csv("Agric/AGSEC2A.csv") %>%
  dplyr::select(hhid, parcelID) %>%
  distinct(hhid, parcelID, .keep_all = T)

# GSEC2: Survey roster
# hhid
# PID: person ID
# h2q3: sex (1=M, 2=F)
dc <- read.csv("HH/GSEC2.csv") %>% 
  dplyr::select(hhid, PID, h2q3) %>%
  group_by(hhid, PID) %>%
  summarise(h2q3 = first(h2q3), .groups = "drop")

# AGSEC3A: Labour and input use
# hhid
# parcelID
# pltid: plot ID
# s3aq03_3: primary decision maker
db <- read.csv("Agric/AGSEC3A.csv") %>% 
  dplyr::select(hhid, parcelID, pltid, s3aq03_3) %>%
  rename(PID = s3aq03_3) %>%
  left_join(dc %>% rename(female = h2q3), by = c("hhid", "PID")) %>%
  mutate(female = ifelse(female == 2, 1, 0))

db <- db %>%
  group_by(hhid, parcelID, pltid) %>%
  summarise(PID = first(PID), female = first(female), .groups = "drop")

# AGSEC4A: Crops grown
# hhid
# parcelID
# pltid
# cropID
# s4aq07: total area of crop planted
dd <- read.csv("Agric/AGSEC4A.csv") %>%
  dplyr::select(hhid, parcelID, pltid, cropID, s4aq07) %>%
  group_by(hhid, parcelID, pltid, cropID) %>%
  summarise(s4aq07 = sum(s4aq07, na.rm = T), .groups = "drop")

# AGSEC5A: Quantification of production
# hhid
# parcelID
# pltid
# cropID
# s5aq08_1: value of harvest
de <- read.csv("Agric/AGSEC5A.csv") %>%
  dplyr::select(hhid, parcelID, pltid, cropID, s5aq08_1) %>%
  group_by(hhid, parcelID, pltid, cropID) %>%
  summarise(s5aq08_1 = sum(s5aq08_1, na.rm = T), .groups = "drop")


# Cleaning and stuff ------------------------------------------------------

# Merge in crops, output, and parcel info
df <- db %>%
  inner_join(dd, by = c("hhid", "parcelID", "pltid")) %>%
  left_join(de, by = c("hhid", "parcelID", "pltid", "cropID")) %>%
  left_join(da, by = c("hhid", "parcelID")) %>%
  mutate(yield = s5aq08_1 / s4aq07, lnvalue = log(1 + s5aq08_1), lnarea = log(1 + s4aq07)) %>%
  filter(!is.na(female), !is.na(yield))

df <- df %>% rename(HHID = hhid)

# Trying to understand what's in the final df
df %>% summarise(across(everything(), ~n_distinct(.)))

write.csv(df, "C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2018_2019_data.csv", row.names = F)




