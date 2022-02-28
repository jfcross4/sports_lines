library(lubridate)
library(dplyr)
library(ggplot2)

nfl <- read.csv("NFL_Lines_09-19.csv")
vegas <- read.csv("VegasLines2000-2017.csv")

# test code added



nfl %>% filter(season==2017, type=="Week 1", home.team=="NE")
vegas %>% filter(season==2017, type=="Week 1", home.team=="New England")

vegas <- vegas %>% mutate(actual.total = home.score + away.score)
vegas <- vegas %>% mutate(date2 = as.Date(date))
vegas <- vegas %>% mutate(residual = actual.total - over.under)
vegas <- vegas %>% mutate(over = over.under>actual.total)
vegas <- vegas %>% mutate(over_or_tie = over.under>=actual.total)
vegas <- vegas %>% mutate(under = over.under<actual.total)

vegas <- vegas %>% mutate(high.over.under = (over.under>=43))


library(ggplot2)

vegas %>% ggplot(aes(over.under, actual.total))+geom_point()+
  geom_smooth(method="lm")

## making predictions

season_for_ratings <- 2017
vegas_season <- vegas  %>% filter(season == season_for_ratings)
vegas_season <- vegas_season %>% 
  dplyr::select(date, type, home.team, away.team, home.score, away.score)


vegas_season_home <- vegas_season
vegas_season_away <- vegas_season

vegas_season_home <- vegas_season_home %>% rename(team = home.team, opp=away.team,
                             score=home.score, opp.score = away.score) %>%
  mutate(home =1)

vegas_season_away <- vegas_season_away %>% 
  rename(team = away.team, opp=home.team,
       score=away.score, opp.score = home.score) %>%
  mutate(home =0)

vegas_season_away <- vegas_season_away[,colnames(vegas_season_home)]

vegas_season <- rbind(vegas_season_home, vegas_season_away)

m.off <- lm(score ~ team + opp, data=vegas_season)
summary(m.off)



library(lme4)

m.regressed <- lmer(score ~ (1|team) + (1|opp), data=vegas_season)
summary(m.regressed)
coef(m.regressed)

vegas_season <- vegas_season %>% mutate(score_diff = score-opp.score)

m.regressed <- lmer(score_diff ~ (1|team) + (1|opp), data=vegas_season)
summary(m.regressed)
coef(m.regressed)
