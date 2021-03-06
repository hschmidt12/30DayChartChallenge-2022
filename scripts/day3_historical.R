# 30 Day Chart Challenge - 2022
# day 3 - historical (comparisons)
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/30DayChartChallenge-2022")
# data from U.S. census bureau https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
# read in data
df <- read.csv("./data/apportionment.csv")
set.seed(123)

# load packages
library(tidyverse)
library(showtext)
library(RColorBrewer)
library(ggrepel)
library(scales)

# load fonts for title
font_add_google("Arvo", "arvo")
# automatically use showtext when needed
showtext_auto()

# generate state colors (navy, pink, beige, blue)
region.palette <- c("#2d4057","#fa9483","#f3deca","#4097aa")
palette <- colorRampPalette(c("#fa9483","#4097aa"))
palette <- palette(52)

# make resident population variable numeric
df$Resident.Population <- as.numeric(gsub(",","",df$Resident.Population))
# make population in millions units
df$Resident.Population <- df$Resident.Population/1000000

# filter data by name, geography type, year, and population
df.states <- df %>%
  select(Name, Year, Resident.Population, Geography.Type) %>%
  filter(Geography.Type == "State")

df.regions <- df %>%
  select(Name, Year, Resident.Population, Geography.Type) %>%
  filter(Geography.Type == "Region")

# plot line graph showing state populations over time
df.states %>%
  mutate(label = if_else(Year == max(Year) & Resident.Population >= 10, as.character(Name), NA_character_)) %>%
  ggplot(aes(x = Year, y = Resident.Population, group = Name, color = Name)) +
  geom_line(aes(color = Name), size = 1) +
  theme_classic() +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  geom_label_repel(aes(label = label),
                   nudge_x = 10,
                   na.rm = T) +
  labs(title = "U.S. State Populations from 1910 - 2020") +
  ylab("Resident Population (millions)") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        panel.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        text = element_text(family = "arvo", size = 7),
        plot.title = element_text(
          family = "arvo", 
          size = 12,
          color = "#2d4057", 
          hjust = 0.5))
              
ggsave(filename = "./charts/day3_historical1.jpeg",
       width = 3.5,
       height = 3,
       device = "jpeg")

# plot area graph showing region populations over time
ggplot(df.regions, aes(x = Year, y = Resident.Population, group = Name)) +
  geom_area(aes(fill = Name)) +
  theme_classic() +
  scale_fill_manual(values = region.palette) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  labs(title = "U.S. Region Populations from 1910 - 2020") +
  ylab("Resident Population (millions)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        panel.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        legend.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        legend.key.height = unit(0.25, "cm"), 
        legend.text = element_text(size = 4),
        text = element_text(family = "arvo", size = 7),
        plot.title = element_text(
          family = "arvo", 
          size = 11,
          color = "#2d4057", 
          hjust = 0.5))

ggsave(filename = "./charts/day3_historical2.jpeg",
       width = 3.5,
       height = 3,
       device = "jpeg")
