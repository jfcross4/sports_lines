prep_play_by_play_data <- function(){
  data <- nflfastR::load_pbp(2007:2021)
  
  data_orig <- data %>% filter(two_point_attempt==0) %>%
    filter(!(play_type %in% c("no_play", "kickoff", "extra_point")))
  
  data_slim <- data %>% filter(two_point_attempt==0) %>%
    filter(!(play_type %in% c("no_play", "kickoff", "extra_point"))) %>%
    dplyr::select(game_id, posteam, away_score, home_score, score_differential, 
                  game_seconds_remaining, yardline_100, posteam_type, down, ydstogo, wp)
  
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
           win_pos_team = ifelse(posteam_type=="home", win_home_team, 1-win_home_team)
    )
  
  
  return(data_slim)
  
}
