library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(rpart)
library(rpart.plot)
library(randomForest)
#library(nflreadr) # for schedules



# dplyr::glimpse(data)
# hist(data$wp)

## add variables (spread, timeouts, ball in 2nd half, home team, time remaining in half, 
## ratio of point differential)
# https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/










# score differential is from possession teams point of view (at the time of the play)
# posteam_type says whether the team with possesion is the home or away team

# max depth is how how many layers the tree has

# freddie <- rpart(win_pos_team ~ score_differential+
#                    game_seconds_remaining + yardline_100, 
#                  data=data_slim, maxdepth=8, cp=0)
# prp(freddie)
# data_slim$predictions <- predict(freddie, data_slim)
# 
# data_slim %>% ggplot(aes(as.factor(predictions, win_pos_team)))+
#   geom_jitter(size=0.1)+geom_smooth(method="lm")
# 
#   data_slim %>% group_by(predictions) %>% 
#     summarize(n=n(), mean(win_pos_team))
  


# data_slim %>% summarize(AAE(predictions, win_pos_team)) #0.319
# 
# # best we could have done
# data_slim %>% summarize(mean(2*(predictions*(1-predictions)))) #0.319
# 
# # worst we could have (realistically) done
# data_slim %>% summarize(AAE(win_pos_team, 0.5))  #0.5
# 
# 
# data_slim %>% summarize(RMSE(predictions, win_pos_team)) #0.399
# 
# 


# worst we could have (realistically) done
# data_slim %>% summarize(RMSE(win_pos_team, 0.5))  #0.5
# 
# nrow(data_slim) #37142

sample_rows <- sample(1:nrow(data_slim), 
                      size=round(nrow(data_slim)/2), replace=FALSE)


train = data_slim[sample_rows, ]
test = data_slim[-sample_rows, ]

# freddie <- rpart(win_pos_team ~ score_differential+
#                    game_seconds_remaining + yardline_100, 
#                  data=train, maxdepth=30, cp=0.001)
# 
# freddie <- rpart(win_pos_team ~ ., 
#                  data=train, maxdepth=3, cp=0.001)
# 
# 
# prp(freddie)

# test$predictions <- predict(freddie, test)
# 
# test %>% summarize(AAE(predictions, win_pos_team)) #0.318
# 
# # best we could have done
# test %>% summarize(mean(2*(predictions*(1-predictions)))) #0.316
# 
# # worst we could have (realistically) done
# test %>% summarize(AAE(win_pos_team, 0.5))  #0.5


# test %>% summarize(RMSE(predictions, win_pos_team)) #0.401
# 
# test %>% summarize(RMSE(0.5, win_pos_team)) #0.5
# one_game <- data_slim %>% filter(game_id == "2019_01_ATL_MIN")
# 




t <- Sys.time()
rf <- randomForest(win_pos_team~score_differential+
                     game_seconds_remaining + yardline_100 + 
                     down+ydstogo+spread_line, data=train, mtry=3,
                   ntree=30,
                   na.action=na.omit, nodesize=100)
Sys.time()-t

hist(predict(rf, newdata = test))
varImpPlot(rf,type=2)

test$predictions_rf = predict(rf, newdata = test)

test <- test %>% filter(!is.na(predictions), !is.na(wp))

RMSE(test$predictions, test$win_pos_team)
RMSE(test$wp, test$win_pos_team) #0.4011665

ggplot(test, aes(predictions, wp))+geom_point()+geom_smooth()


data_pred = train %>% dplyr::select(score_differential,
                         game_seconds_remaining, yardline_100,  
                         down, ydstogo, spread_line)

data_test = test %>% dplyr::select(score_differential,
                                   game_seconds_remaining, yardline_100,  
                                   down, ydstogo, spread_line) %>% as.matrix()


library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(data_pred), 
                      label = train$win_pos_team)


bst <- xgboost(data = dtrain, max.depth = 3, eta = 0.3, 
               nthread = 2, nrounds = 600, 
               objective = "binary:logistic", verbose = 1)


test$predictions <- predict(bst, data_test)


RMSE(test$predictions, test$win_pos_team)
RMSE(test$wp, test$win_pos_team)

AAE(test$predictions, test$win_pos_team)

#0.4044245 Random Forest
#0.4036977 Xgboost, maxdepth=3, nrounds =400, eta = 0.5
#0.4041334 Xgboost, maxdepth=3, nrounds =600, eta = 0.5
#0.4033112 Xgboost, maxdepth=3, nrounds =600, eta = 0.3



with(test, cor(predictions, predictions_rf)) # our two model 0.976
with(test, cor(predictions, wp)) # our xgboost and their xgboost: 0.979
with(test, cor(predictions_rf, wp)) # our rf and their xgboost 0.970


RMSE(0.7*test$predictions + 0.3*test$predictions_rf, test$win_pos_team)

lm(win_pos_team ~ predictions_rf + predictions + wp, data=test)


# use vegas info going into the game (the spread or the money line)

cv <- xgb.cv(data = dtrain, nrounds = 600, nthread = 2, 
             nfold = 5, metrics = list("rmse","auc"),
             max_depth = 3, eta = 0.3, objective = "binary:logistic",
             prediction=TRUE)

min_rmse_index  = which.min(cv$evaluation_log$test_rmse_mean)
min_rmse <-  cv$evaluation_log[min_rmse_index]$test_rmse_mean

plot(1:600, cv$evaluation_log$test_rmse_mean, 
     ylim=c(0.401, 0.405), ylab="Test RMSE", xlab="nrounds", 
     main="xgboost with eta=0.3 and max_depth=3, nfold=5")
# best 0.4020158 with 140 rounds



cv <- xgb.cv(data = dtrain, nrounds = 600, nthread = 2, 
             nfold = 5, metrics = list("rmse","auc"),
             max_depth = 3, eta = 0.1, objective = "binary:logistic",
             prediction=TRUE)
# best 0.4019126 with 412 rounds


