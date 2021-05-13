library(tidyverse)
library(tidytuesdayR)
library(scales)
library(lubridate)
library(countrycode)
library(patchwork)
library(maps)
library(ggthemes)
library(gridExtra)



## Load the Data
tt <- tt_load("2021-05-04")

water <- tt$water %>% 
  mutate(report_date = mdy(report_date)) %>% 
  rename(lat = lat_deg,
         lon = lon_deg,
         country = country_name) %>% 
  separate(water_tech, c("water tech", "brand"), sep = "-", fill = "right") %>% 
  mutate(install_year = ifelse(install_year>2021, NA_real_, install_year)) %>% 
  filter(!country%in% c("Peru", "Dominican Republic", "Timor-Leste"),
         !is.na(country)) %>% 
  filter(between(lat,-35,35),
         between(lon,-40,60))

# Uganda

water_uganda <- water %>% 
  filter(country == "Uganda", 
         between(lat,-2,4),
         between(lon,29.5,35.1))

water_uganda_lumped <- water_uganda %>% 
  mutate(water_source = fct_lump(water_source, 5)) %>% 
  replace_na(list(water_source = "Other"))

water_uganda_map <- water_uganda_lumped %>% 
  ggplot(aes(lon,lat, color = water_source))+ 
  borders("world", regions = "Uganda", fill = "#fed3b9", colour = "#722900")+ 
  geom_point(size = .5, alpha = .25) + 
  theme_map() + 
  theme(panel.background = element_rect(fill = "#fff7f2",
                                        colour = "#fff7f2",
                                        size = 0.5, linetype = "solid"))+
  scale_color_manual(values = c("#651fff", "#6d2aff", "#7c40ff", "#9b6dff", "#b99aff","#d8c7ff"),
                     guide = guide_legend(override.aes = list(size = 4, alpha = 1)),name = "Water Sources in Uganda")+ 
  theme(legend.position = "right", legend.key = element_rect(fill = "#fff7f2"))

# Nigeria
water_nigeria <- water %>% 
  replace_na(list(water_source = "Other")) %>% 
  filter(country == "Nigeria",
         between(lat,4,13),
         between(lon,2,16))%>% 
  ggplot(aes(lon,lat, color = water_source))+ 
  borders("world", regions = "Nigeria", fill = "#fed3b9", colour = "#722900")+ 
  geom_point(size = .5, alpha = .1) + 
  theme_map() + 
  theme(panel.background = element_rect(fill = "#fff7f2",
                                        colour = "#fff7f2",
                                        size = 0.5, linetype = "solid"))+
  scale_color_manual(values = c("#44bbff","#44ddff","#44ddff","#44eeff","#44eedd","#44eebb"), 
                     guide = guide_legend(override.aes = list(size = 4, alpha = 25)),name = "Water Sources in Nigeria")+ 
  theme(legend.position = "right", legend.title = element_blank(),legend.key = element_rect(fill = "#fff7f2"))

# Liberia

water_liberia <- water %>% 
  filter(country == "Liberia") %>% 
  mutate(water_source = fct_lump(water_source, 5)) %>% 
  replace_na(list(water_source = "Other")) %>% 
  ggplot(aes(lon,lat, color = water_source))+ 
  borders("world", regions = "Liberia", fill = "#fed3b9", colour = "#722900")+ 
  geom_point(size = .5, alpha = .25) + 
  theme_map() + 
  theme(panel.background = element_rect(fill = "#fff7f2",
                                        colour = "#fff7f2",
                                        size = 0.5, linetype = "solid"))+
  scale_color_manual(values = c("#c90c8f", "#eb0ea7", "#f12bb4", "#f34dc0", "#f56fcc","#f890d8"),
                     guide = guide_legend(override.aes = list(size = 4, alpha = 1)),name = "Water Sources in Liberia") + 
  theme(legend.position = "right", legend.key = element_rect(fill = "#fff7f2"))

# Ghana

water_ghana <- water %>% 
  filter(country == "Ghana") %>% 
  replace_na(list(water_source = "Other")) %>% 
  ggplot(aes(lon,lat, color = water_source))+ 
  borders("world", regions = "Ghana", fill = "#fed3b9", colour = "#722900")+ 
  geom_point(size = .5, alpha = .25) + 
  theme_map() + 
  theme(panel.background = element_rect(fill = "#fff7f2",
                                        colour = "#fff7f2",
                                        size = 0.5, linetype = "solid"))+
  scale_color_manual(values = c("#15bf83", "#1ee6a0", "#4aebb3", "#77efc5", "#a3f4d8","#cff9ea"),
                     guide = guide_legend(override.aes = list(size = 4, alpha = 1)), name = "Water Sources in Ghana")+ 
  theme(legend.position = "right",legend.key = element_rect(fill = "#fff7f2"))

(water_uganda_map + water_ghana + water_liberia + water_nigeria) + 
  theme(panel.background = element_rect(fill = "#fff7f2",
                                        colour = "#fff7f2",
                                        size = 0.5, linetype = "solid"))

grid.arrange(water_uganda_map,water_ghana, water_liberia,water_nigeria)
