load("data_with_rf_predictions.RData")
load("data_with_rf_predictions2.RData")

head(data_slim)

game = "2021_22_LA_CIN"

game_data = data_slim %>% 
  filter(game_id == game)

TEAM = "LA"

game_data = game_data %>% 
  mutate(rf_win_prob_team = 
         ifelse(posteam==TEAM,predictions_rf, 1-predictions_rf ))

game_data = game_data %>% 
  mutate(wp_win_prob_team = 
           ifelse(posteam==TEAM,wp, 1-wp))



game_data %>% 
  mutate(time_elapsed = 
           3600-game_seconds_remaining) %>%
  ggplot(aes(time_elapsed, 
             rf_win_prob_team))+geom_line()

game_data %>% 
  mutate(time_elapsed = 
           3600-game_seconds_remaining) %>%
  ggplot(aes(time_elapsed, 
             wp_win_prob_team))+geom_line()

game_data %>% arrange(desc(game_seconds_remaining)) %>% 
  mutate(wp_diff_our_model = 
           rf_win_prob_team-lag(rf_win_prob_team, k=1),
         wp_diff_their_model = 
           wp_win_prob_team-lag(wp_win_prob_team, k=1)
         ) %>% ggplot(aes(wp_diff_our_model, 
                          wp_diff_their_model)) +
  geom_point()+geom_smooth(method="lm")

game_data %>% arrange(desc(game_seconds_remaining)) %>% 
  mutate(wp_diff_our_model = 
           rf_win_prob_team-lag(rf_win_prob_team, k=1),
         wp_diff_their_model = 
           wp_win_prob_team-lag(wp_win_prob_team, k=1)
  ) %>% View()

game_data = game_data %>% arrange(desc(game_seconds_remaining)) %>% 
  mutate(wp_diff_our_model = 
           rf_win_prob_team-lag(rf_win_prob_team, k=1),
         wp_diff_their_model = 
           wp_win_prob_team-lag(wp_win_prob_team, k=1)
  )

game_data %>% summarize(sd(wp_diff_our_model, na.rm = TRUE), 
                        sd(wp_diff_their_model, na.rm = TRUE))

game_data %>% summarize(cor(wp_diff_our_model, lag(wp_diff_our_model), 
use = "pairwise.complete"))

# 
# `sd(wp_diff_our_model, na.rm = TRUE)` `sd(wp_diff_their_model, na.rm = TRUE)`
# <dbl>                                   <dbl>
#   1                                0.0813                                  0.0480


# # A tibble: 1 × 1
# `cor(wp_diff_our_model, lag(wp_diff_our_model), use = "pairwise.…
#                                                               <dbl>
# 1                                                            -0.162
