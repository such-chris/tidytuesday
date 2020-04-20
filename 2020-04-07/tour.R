###
# Tidy Tuesday - Le Tour
# April 07, 2020
# Author: Chris Suchocki @wow_such_chris

library(tidyverse)
library(lubridate)
library(RColorBrewer)

# read in the data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')


tdf_winners <- mutate(tdf_winners, 
                      year = year(start_date),
                      speed = distance / time_overall,
                      group = "not important", )

tdf_short <- select(tdf_winners, 
                    year, 
                    winner_name, 
                    time_overall, 
                    distance,
                    speed,
                    nationality,
                    group)

tdf_short$group[tdf_short$speed > 41.7] <- "important"

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

exp1 = list("1915 - 1918 tours", "cancelled due to WWI")
exp2 = list("1940 - 1946 tours", "cancelled due to WWII")

palette <- c("#000000", "FFCC33")


ggplot(data = tdf_short, mapping = aes(x = year, y = time_overall, fill = group)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1900, 2020, 1),
                     labels = every_nth(seq(1900, 2020, 1), 
                                        10, 
                                        inverse = TRUE)) +
  theme_classic() +
  labs(title = "Tour de France Finish Times", 
       x = "Date",
       y = "Finish Time (Hours)") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = -45, 
                                   hjust = 0.05, 
                                   vjust = 0.3),
        axis.title.y = element_text(vjust = 1.5),
        legend.position = "none") +
  geom_segment(aes(x = 1914.5, y = 190, xend = 1914.5, yend = 235)) +
  geom_segment(aes(x = 1918.5, y = 190, xend = 1918.5, yend = 235)) +
  geom_segment(aes(x = 1914.5, y = 190, xend = 1918.5, yend = 190)) +
  geom_segment(aes(x = 1916.5, y = 190, xend = 1916.5, yend = 160)) +
  annotate("text", x = 1916.5, y = c(158, 154), label = exp1, size = 2.3) +
  geom_segment(aes(x = 1939.5, y = 130, xend = 1939.5, yend = 160)) +
  geom_segment(aes(x = 1946.5, y = 130, xend = 1946.5, yend = 160)) +
  geom_segment(aes(x = 1939.5, y = 160, xend = 1946.5, yend = 160)) +
  geom_segment(aes(x = 1943, y = 160, xend = 1943, yend = 190)) +
  annotate("text", x = 1943.5, y = c(197, 193), label = exp2, size = 2.3) +
  scale_fill_manual(values = c("#000000", "#FFCC33"))
