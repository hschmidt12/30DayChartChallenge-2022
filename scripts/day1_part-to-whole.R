# 30 Day Chart Challenge - 2022
# day 1 - part-to-whole (comparisons)
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/30DayChartChallenge-2022")
# get list of senators (obtained from https://www.senate.gov/senators/)
senators <- read.csv("./data/2022-us-senators.csv")

# load packages
library(tidyverse)
library(showtext)

# load fonts for title
font_add_google("Lobster Two", "lobstertwo")
# automatically use showtext when needed
showtext_auto()

# set custom colors
colors <- c("#1e4558","#ffb049","#d52027","#f8eee3")

# sort by party
senators <- senators[order(senators$party),]

# compute percentages
senators <- senators %>%
  group_by(party) %>%
    mutate(fraction = length(party)/100)

# compute the cumulative percentages (top of each rectangle)
senators$ymax = cumsum(senators$fraction)
# Compute the bottom of each rectangle
senators$ymin = c(0, head(senators$ymax, n=-1))

# Compute label position for each party
senators <- senators %>%
  group_by(party) %>%
  mutate(labelPosition = (sum(ymax) + sum(ymin))/100)
# fix independent party label position
senators$labelPosition[senators$party == "Independent"] <- 23
# define labels
senators <- senators %>%
  group_by(party) %>%
    mutate(label = paste0(party, " (", length(name), ")"))

# plot!
ggplot(senators, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=party)) +
  geom_rect() +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors[4]) +
  geom_label(x=3.5, aes(y=labelPosition, label=label, color = colors[4]), size=4) +
  coord_polar(theta="y") + 
  xlim(c(1, 4)) +
  theme_void() +
  labs(title = "Party Affiliations of 2022 U.S. Senate") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#f8eee3", color = "#f8eee3"),
        panel.background = element_rect(fill = "#f8eee3", color = "#f8eee3"),
        plot.title = element_text(
          family = "lobstertwo", 
          size = 24,
          face = "bold", 
          color = colors[1], 
          hjust = 0.5,
          vjust = -1.5))

# save plot
ggsave(filename = "./charts/day1_part-to-whole.jpeg",
       width = 5.5,
       height = 5,
       device = "jpeg")
