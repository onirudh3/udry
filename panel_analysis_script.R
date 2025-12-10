
# Libraries
library(dplyr) # select(), %>%
library(fixest) # feols()

# Forbid scientific notation
options(scipen = 999)


# Preliminaries -----------------------------------------------------------

# Checking if the 2010-11 to 2011-12 HHID values are stable in the raw data
hh10 <- unique(read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/UGA_2010_UNPS_v03_M_CSV/GSEC1.csv")$HHID)
length(hh10) # 2716

hh11 <- unique(read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/UGA_2011_UNPS_v02_M_CSV/GSEC1.csv")$HHID)
length(hh11) # 2850

# How many values are common between the two
length(intersect(hh10, hh11)) # 2510 so it is a good sign


# Construct panel ---------------------------------------------------------

# 2010-11
da <- read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2010_2011_data.csv")
da %>% summarise(across(everything(), ~n_distinct(.))) # to get counts of unique IDs just to see if everything looks okay

# 2011-12
db <- read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2011_2012_data.csv")
db %>% summarise(across(everything(), ~n_distinct(.)))

# How many HHIDs are common between the two cleaned datasets
common_hh <- intersect(unique(da$HHID), unique(db$HHID))
length(common_hh) # 971 which isn't too bad

# Keep only those observations for those common HHIDs
da <- da %>% filter(HHID %in% common_hh) %>% mutate(year = 2010)
db <- db %>% filter(HHID %in% common_hh) %>% mutate(year = 2011)

# Construct panel
df <- bind_rows(da, db) %>% mutate(hh_crop = interaction(HHID, cropID, drop = T),
                                   hh_year_crop = interaction(HHID, year, cropID, drop = T))
df %>% summarise(across(everything(), ~n_distinct(.)))


# Regression --------------------------------------------------------------

# Gender
summary(feols(lnvalue ~ female | hh_year_crop, df))

# Gender + lnarea
summary(feols(lnvalue ~ female + lnarea | hh_year_crop, df))





