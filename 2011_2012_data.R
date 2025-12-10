# Libraries
library(dplyr) # select(), %>%

# Working directory
setwd("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/UGA_2011_UNPS_v02_M_CSV")


# 2011-12 Data ------------------------------------------------------------

# Same cleaning process
da <- read.csv("AGSEC2A.csv") %>% 
  dplyr::select("HHID", "parcelID")

db <- read.csv("AGSEC3A.csv") %>% 
  dplyr::select("HHID", "parcelID", "plotID", "a3aq3_3")

dc <- read.csv("GSEC2.csv") %>% 
  dplyr::select("HHID", "PID", "h2q3")

dd <- read.csv("AGSEC4A.csv") %>% 
  dplyr::select("HHID", "parcelID", "plotID", "cropID", "a4aq7")

de <- read.csv("AGSEC5A.csv") %>% 
  dplyr::select("HHID", "parcelID", "plotID", "cropID", "a5aq8") %>%
  group_by(HHID, parcelID, plotID, cropID) %>%
  summarise(a5aq8 = sum(a5aq8, na.rm = T), .groups = "drop")

db <- db %>% 
  rename(PID = a3aq3_3) %>%
  left_join(dc %>% rename(female = h2q3), by = c("HHID", "PID")) %>% 
  mutate(female = ifelse(female == 2, 1, 0))

df <- db %>%
  inner_join(dd, by = c("HHID", "parcelID", "plotID")) %>%
  left_join(de, by = c("HHID", "parcelID", "plotID", "cropID")) %>%
  left_join(da, by = c("HHID", "parcelID")) %>%
  mutate(yield = a5aq8 / a4aq7, lnvalue = log(1 + a5aq8), lnarea = log(1 + a4aq7)) %>%
  filter(!is.na(female), !is.na(yield))

df <- df %>% mutate(year = 2011)

write.csv(df, "C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2011_2012_data.csv", row.names = F)
