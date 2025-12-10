
# Libraries
library(dplyr) # select(), %>%
library(fixest) # feols()

# Forbid scientific notation
options(scipen = 999)


# Bind all data -----------------------------------------------------------

# 2010-11
da <- read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2010_2011_data.csv") %>% 
  dplyr::select("HHID", "cropID", "female", "lnvalue", "lnarea") %>% 
  mutate(year = 2010)

# 2011-12
db <- read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2011_2012_data.csv") %>% 
  dplyr::select("HHID", "cropID", "female", "lnvalue", "lnarea") %>% 
  mutate(year = 2011)

# 2018-19
dc <- read.csv("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2018_2019_data.csv") %>% 
  dplyr::select("HHID", "cropID", "female", "lnvalue", "lnarea") %>% 
  mutate(year = 2018)

# Bind all
df <- rbind(da, db, dc) %>% mutate(hh_year_crop = interaction(HHID, year, cropID, drop = T))

# Is there anything weird
df %>% summarise(across(everything(), ~n_distinct(.)))


# Regression --------------------------------------------------------------

# Gender
summary(feols(lnvalue ~ female | hh_year_crop, df))

# Gender + lnarea
summary(feols(lnvalue ~ female + lnarea | hh_year_crop, df))



