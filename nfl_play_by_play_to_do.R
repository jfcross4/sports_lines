### to do
### read in 2021 data
### pick out one game, filter data to get only that game
### (optional) assign numbers to plays in order
library(readr); library(dplyr)


pbp_2021 <- read_csv("~/Downloads/pbp-2021.csv")

pbp_2021 <- pbp_2021 %>%
  mutate(min_remaining = 60 - (Quarter * 15) + Minute + (Second / 60))

pbp_2021 <- pbp_2021 %>% group_by(GameId) %>% arrange(desc(min_remaining)) %>%
  mutate(playId = row_number())

DALvTB <- pbp_2021 %>%filter(GameId == "2021090900")

DALvTB %>% arrange(min_remaining) %>%
  mutate(playId = row_number())

DALvTB <- DALvTB %>% arrange(min_remaining) %>%
  mutate(playId = row_number())

