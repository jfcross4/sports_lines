install.packages("nflfastR")

if (!require("remotes")) install.packages("remotes")
remotes::install_github("nflverse/nflfastR")

# install.packages("tidyverse", type = "binary")
# install.packages("ggrepel", type = "binary")
# install.packages("ggimage", type = "binary")
# install.packages("nflfastR", type = "binary")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

data <- load_pbp(2019)
View(data)

library(rpart)
library(rpart.plot)

data_slim <- data %>% dplyr::select(away_score, home_score, score_differential, 
                                    game_seconds_remaining, yardline_100, posteam_type, down, ydstogo)

data_slim <- data_slim %>% mutate(win = 1*(home_score > away_score), diff = home_score - away_score)

# score differential is from possession teams point of view

freddie <- rpart(win ~ score_differential+posteam_type, data=data_slim, maxdepth=2, cp=0)
prp(freddie)

one_game <- data %>% mutate(game_id = "2019_01_ATL_MIN")
