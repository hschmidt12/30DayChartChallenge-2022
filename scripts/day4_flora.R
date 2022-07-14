# 30 Day Chart Challenge - 2022
# day 4 - flora (comparisons)
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/30DayChartChallenge-2022")

# load packages
library(tidyverse)
library(sf)
library(showtext)

# load fonts for title
font_add_google("Satisfy", "satisfy")
# automatically use showtext when needed
showtext_auto()

# Read in Philadelphia base map
base <- read_sf("./data/philadelphia-base/Street_Centerline.shp") %>%
  select(geometry) 

# Read in Philadelphia tree data
# points for trees > 6' in diameter
trees <- read_sf("./data/tree-canopy-points/ppr_tree_canopy_points_2015.shp") %>%
  select(geometry)

ggplot() + geom_sf(data = base$geometry, color = "#dbdbdb", size = 0.08) +
  geom_sf(data = trees, size=.3, color='#98ce00') +
  theme_void() +
  labs(title = "2015 Philadelphia \nTree Canopy") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#315659", color = "#315659"),
        plot.background = element_rect(fill = "#315659", color = "#315659"),
        plot.title = element_text(
          family = "satisfy", 
          size = 14,
          face = "bold", 
          color = "white", 
          hjust = 0.1,
          vjust = -2))

# save plot
ggsave(filename = "./charts/day4_flora.jpeg",
       width = 3.5,
       height = 3,
       device = "jpeg")
