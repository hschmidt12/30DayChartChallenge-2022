# 30 Day Chart Challenge - 2022
# day 5 - slope (comparisons)
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/30DayChartChallenge-2022")

# load packages
library(tidyverse)
library(showtext)
library(scales)

# load fonts for title
font_add_google("Oswald", "oswald")
# automatically use showtext when needed
showtext_auto()

# make mbta color palette (blue, green, orange, red)
colors <- c("#003DA5","#00843D","#ED8B00","#DA291C")

# load MBTA ridership data (https://mbta-massdot.opendata.arcgis.com/)
mbta <- read.csv("./data/MBTA_Monthly_Ridership_by_Mode.csv") %>%
  filter(mode == "Rail") # select subway lines only

# make a new numeric year column
mbta$year <- substr(mbta$service_date, 1, 4)
mbta$year <- as.numeric(mbta$year)

# calculate average monthly ridership per year, per line
mbta.avg <- mbta %>%
  group_by(route_or_line, year) %>%
  summarize(avg.ridership = mean(total_monthly_ridership))
# change units to thousands of riders
mbta.avg$avg.ridership <- mbta.avg$avg.ridership/1000

# plot!
ggplot(data = mbta.avg, aes(x = year, y = avg.ridership, 
                            group = route_or_line, color = route_or_line)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  labs(title = "Boston\nMBTA\nSubway\nRidership") +
  xlab(NULL) + ylab("Average Ridership (thousands)") +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
        plot.title = element_text(
          family = "oswald", 
          size = 10,
          face = "bold", 
          color = "black", 
          hjust = 0.9,
          vjust = -4))

# save plot
ggsave(filename = "./charts/day5_slope.jpeg",
       width = 3.5,
       height = 3,
       device = "jpeg")
