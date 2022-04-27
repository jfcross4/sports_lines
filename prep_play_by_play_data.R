prep_play_by_play_data <- function(){
  data <- nflfastR::load_pbp(2007:2021)
  
  data_slim <- data %>% filter(two_point_attempt==0) %>%
    filter(!(play_type %in% c("no_play", "kickoff", "extra_point"))) %>%
    dplyr::select(game_id, posteam, away_score, home_score, score_differential, 
                  game_seconds_remaining, yardline_100, posteam_type, down, ydstogo, wp,
                  posteam_timeouts_remaining, defteam_timeouts_remaining, 
                  half_seconds_remaining)
  
  source("add_second_half_possesion.R")
  
  data_slim = add_second_half_possesion(data_slim)
  
  data_slim = data_slim %>% mutate(posteam = as.character(posteam),
                                   posteam = case_when(
                                     posteam ==  "STL"~ "LA",
                                     posteam == "SD"~ "LAC",
                                     posteam ==  "OAK" ~ "LV",
                                     !(posteam %in% c("STL", "SD", "OAK")) ~ posteam
                                   )
  )
  
  source("get_spreads.R")
  
  spreads = get_spreads()
  
  data_slim = left_join(data_slim, 
                        spreads,
                        by=c("game_id", "posteam"="team")
  )
  
  data_slim <- data_slim %>% 
    mutate(win_home_team = 1*(home_score > away_score), 
           end_game_diff = home_score - away_score,
           win_pos_team = ifelse(posteam_type=="home", win_home_team, 1-win_home_team),
           diff_time_ratio = score_differential*exp(4*(3600-game_seconds_remaining)/3600),
           spread_time = spread_line*exp(-4*(3600-game_seconds_remaining)/3600)
    )
  
  
  return(data_slim)
  
}
