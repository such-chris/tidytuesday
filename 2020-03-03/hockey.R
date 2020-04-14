###
# Tidy Tuesday - Hockey
# March 03, 2020
# Author: Chris Suchocki @wow_such_chris

library(dplyr)
library(ggplot2)
library(tidyverse)

# get the data (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-03/readme.md)
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')


# clean raw data
goals_by_date <- game_goals %>%
  select(player, date, age, goals, game_num) %>%
  mutate(year = as.numeric(substr(age, 1, 2)),
         days = as.numeric(substr(age, 4, 6))/365.25,
         age = year + days) %>%
  group_by(player) %>%
  mutate(cum_goals = (cumsum(goals))) %>%
  mutate(cumnum_games = 1:n())
  
  ggplot(goals_by_date, aes(x = cumnum_games, y = cum_goals)) +
  geom_line(aes(col=player)) +
  labs(title="Cumulative Goals of Top NHL Players by Game Number", y="Total Goals", x="Game Number") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0,1800,200), minor_breaks = NULL, expand = expand_scale()) +
  scale_y_continuous(breaks = seq(0,900,100), minor_breaks = NULL, expand = expand_scale()) +
  geom_hline(yintercept = 700, size = 0.8, col = 'gray80', lty = 4)