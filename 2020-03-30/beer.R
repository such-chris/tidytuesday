###
# Tidy Tuesday - Beer
# March 30, 2020
# Author: Chris Suchocki @wow_such_chris

library(tidyverse)
library(gridExtra)
library(egg)

# get the data (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md)
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

craft_brewers <- brewer_size %>%
  select(year, brewer_size, 
         n_of_brewers, 
         total_barrels) %>%
  filter(brewer_size %in% c("1 to 1,000 Barrels", "1,001 to 7,500 Barrels", "7,501 to 15,000 Barrels"))


vol_growth <- ggplot(data = craft_brewers, 
                     mapping = aes(x = year, 
                                   y = total_barrels, 
                                   color = brewer_size)) +
  geom_line(size = 0.75, show.legend = FALSE) +
  scale_x_continuous(breaks = c(2009:2019)) +
  scale_y_continuous(labels = c(1, 2, 3, 4)) +
  labs(title = "Beer Production", 
       x = "Year", 
       y = "Total Barrels (millions)") +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.title = element_text(size = 9)) +
  scale_color_manual(values = c("skyblue", "goldenrod", "black"))


n_growth <- ggplot(data = craft_brewers, 
                   mapping = aes(x = year,
                                 y = n_of_brewers, 
                                 color = brewer_size)) +
  geom_line(size = 0.75) +
  scale_x_continuous(breaks = c(2009:2019)) +
  labs(title = "Number of Breweries", 
       x = "Year",
       y = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  theme(axis.title = element_text(size = 9)) +
  scale_color_manual(values = c("skyblue", "goldenrod", "black"),
                     name = "Brewery Size (barrels/year)", 
                     labels = c("1 - 1,000", "1,001 - 7,500", "7,501 - 15,000"))

ggarrange(vol_growth, n_growth, nrow = 1, widths = c(1.5, 1.5), top = "Craft Brewery Growth in the Past Decade")





