# Libraries
library(dplyr) # select(), %>%

# Working directory
setwd("C:/Users/oniru/OneDrive/Development/Econ 590/Homework/UGA_2010_UNPS_v03_M_CSV")


# 2010-11 Data ------------------------------------------------------------

# Same cleaning process
da <- read.csv("AGSEC2A.csv") %>% 
  dplyr::select("HHID", "prcid")

dc <- read.csv("GSEC2.csv") %>% 
  dplyr::select("HHID", "PID", "h2q1", "h2q3") %>%
  mutate(female = ifelse(h2q3 == 0, 1, 0)) %>%
  group_by(HHID, h2q1) %>% 
  summarise(PID = first(PID), female = first(female), .groups = "drop")

db <- read.csv("AGSEC3A.csv") %>% 
  dplyr::select(HHID, prcid, pltid, a3aq40a, a3aq40b, a3aq40c) %>% 
  mutate(line = coalesce(a3aq40a, a3aq40b, a3aq40c)) %>%
  left_join(dc, by = c("HHID", "line" = "h2q1"))

dd <- read.csv("AGSEC4A.csv") %>% 
  dplyr::select("HHID", "prcid", "pltid", "cropID", "a4aq8")

de <- read.csv("AGSEC5A.csv") %>% 
  dplyr::select("HHID", "prcid", "pltid", "cropID", "a5aq8") %>%
  group_by(HHID, prcid, pltid, cropID) %>%
  summarise(a5aq8 = sum(a5aq8, na.rm = T), .groups = "drop")

df <- db %>%
  inner_join(dd, by = c("HHID", "prcid", "pltid")) %>%
  left_join(de, by = c("HHID", "prcid", "pltid", "cropID")) %>%
  left_join(da, by = c("HHID", "prcid")) %>%
  mutate(yield = a5aq8 / a4aq8, lnvalue = log(1 + a5aq8), lnarea = log(1 + a4aq8)) %>%
  filter(!is.na(female), !is.na(yield))

df <- df %>% mutate(year = 2010)

write.csv(df, "C:/Users/oniru/OneDrive/Development/Econ 590/Homework/2010_2011_data.csv", row.names = F)


# Count how many hh-crop pairs
df <- df %>% mutate(hh_crop = interaction(HHID, cropID, drop = T))

df %>%
  group_by(year, hh_crop) %>%
  summarise(n_plots = n(), .groups = "drop") %>%
  group_by(year) %>%
  summarise(total_hh_crop_pairs = n(), multi_plot_pairs = sum(n_plots > 1), share_multi = mean(n_plots > 1))

