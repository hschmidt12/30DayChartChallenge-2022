# 30 Day Chart Challenge - 2022
# day 2 - pictogram (comparisons)
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/30DayChartChallenge-2022")
# read in caffeine data 
# data from https://www.kaggle.com/datasets/heitornunes/caffeine-content-of-drinks
# water category contains caffeinated water beverages
df <- read.csv("./data/caffeine.csv")

# load libraries
library(tidyverse)
library(showtext)
library(magick)
library(rsvg)
library(ggtextures)

# load fonts for title
font_add_google(name = "Kalam", "kalam")
# automatically use showtext when needed
showtext_auto()

# create color palette
colors <- c("#eb4a04","#fe8e36","#265158","#63a99e","#9fddd2","#c5f0e9")

# calculate average caffeine content per drink type
df.avg <- df %>%
  group_by(type) %>%
  summarize(avg.caffeine = mean(Caffeine..mg.))

# add image type to df.avg data frame
df.images <- tibble(
  type = c("Coffee", "Energy\nDrinks", "Energy\nShots", "Soft\nDrinks", "Tea", "Water"),
  avg.caffeine = c(200.58960, 147.86758, 193.41667, 33.67778, 55.86364, 53.73077),
  image = list(
    image_read_svg("./data/images/mug-hot-solid.svg"),
    image_read_svg("./data/images/bolt-solid.svg"),
    image_read_svg("./data/images/whiskey-glass-solid.svg"),
    image_read_svg("./data/images/glass-water-solid.svg"),
    image_read_svg("./data/images/leaf-solid.svg"),
    image_read_svg("./data/images/bottle-water-solid.svg")),
)

# create pictogram
ggplot(df.images, aes(x = type, y = avg.caffeine, image = image, fill = type)) + 
  geom_isotype_col(img_width = grid::unit(0.5,"native"), img_height = NULL,
                   nrow = 20, ncol = 1) + 
  scale_fill_manual(values = colors) +
  theme_classic() +
  labs(title = "Average Caffeine Content (mg) in Different Drinks") +
  xlab(NULL) + ylab("Caffeine (mg)") +
  #coord_flip() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#f5efe8", color = "#f5efe8"),
        panel.background = element_rect(fill = "#f5efe8", color = "#f5efe8"),
        plot.title = element_text(
          family = "kalam", 
          size = 9,
          face = "bold", 
          color = colors[3], 
          hjust = 0.5,
          vjust = -.75))

# save plot
ggsave(filename = "./charts/day2_pictogram.jpeg",
       width = 3.5,
       height = 3,
       device = "jpeg")
  