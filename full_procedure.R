library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(rpart)
library(rpart.plot)
library(randomForest)

source("utils.R")
source("prep_play_by_play_data.R")

data_slim = prep_play_by_play_data()

sample_rows <- sample(1:nrow(data_slim), 
                      size=round(nrow(data_slim)/2), replace=FALSE)


train = data_slim[sample_rows, ]
test = data_slim[-sample_rows, ]

### random forest model on training set
rf <- randomForest(win_pos_team~score_differential+
                     game_seconds_remaining + yardline_100 + 
                     down+ydstogo+spread_line, data=train, mtry=3,
                   ntree=60,
                   na.action=na.omit, nodesize=100)


varImpPlot(rf,type=2)

test$predictions_rf = predict(rf, newdata = test)

data_slim$predictions_rf = predict(rf, newdata = data_slim)
save(data_slim, file="data_with_rf_predictions2.RData")


test <- test %>% filter(!is.na(predictions), !is.na(wp))

RMSE(test$predictions, test$win_pos_team)
RMSE(test$wp, test$win_pos_team)


### xgboost model
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(data_pred), 
                      label = train$win_pos_team)


bst <- xgboost(data = dtrain, max.depth = 3, eta = 0.3, 
               nthread = 2, nrounds = 600, 
               objective = "binary:logistic", verbose = 1)


test$predictions <- predict(bst, data_test)


RMSE(test$predictions, test$win_pos_team)
RMSE(test$wp, test$win_pos_team)

