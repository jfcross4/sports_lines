add_second_half_possesion <- function(data){
  first_pos_team = data %>% 
    group_by(game_id) %>% 
    top_n(1, game_seconds_remaining) %>% 
    dplyr::select(game_id, first_possesion=posteam)
  data = left_join(data, first_pos_team, by="game_id")
  data = data %>% mutate(gets_ball_second_half = posteam != first_possesion)
  return(data)
}

