install.packages("nflfastR")

if (!require("remotes")) install.packages("remotes")
remotes::install_github("nflverse/nflfastR")

install.packages("tidyverse", type = "binary")
install.packages("ggrepel", type = "binary")
install.packages("ggimage", type = "binary")
install.packages("nflfastR", type = "binary")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

data <- load_pbp(2019)
View(data)
