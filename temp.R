nfl <- read.csv("NFL_Lines_09-19.csv")
vegas <- read.csv("VegasLines2000-2017.csv")

# test code added

library(dplyr)

nfl %>% filter(season==2017, type=="Week 1", home.team=="NE")
vegas %>% filter(season==2017, type=="Week 1", home.team=="New England")

vegas <- vegas %>% mutate(actual.total = home.score + away.score)

library(ggplot2)

vegas %>% ggplot(aes(over.under, actual.total))+geom_point()+
  geom_smooth(method="lm")


