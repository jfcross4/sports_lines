get_spreads <- function(){

games <- read.csv("games.csv")

lines <- games %>% 
  dplyr::select(game_id, home_team, away_team, spread_line, total_line)

line_away <- lines %>% select(game_id, team = away_team, spread_line)
line_home <- lines %>% select(game_id, team = home_team, spread_line) %>%
  mutate(spread_line = -1*spread_line)
spreads = rbind(line_away, line_home)

spreads = spreads %>% mutate(team = as.character(team),
                             team = case_when(
                               team ==  "STL"~ "LA",
                               team == "SD"~ "LAC",
                               team ==  "OAK" ~ "LV",
                               !(team %in% c("STL", "SD", "OAK")) ~ team
                             ))
}
