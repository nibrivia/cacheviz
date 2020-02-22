library(tidyverse)
library(shiny)
enableBookmarking(store = "url")
experiments <- read_csv("~/rotorsim/data/experiments.csv") %>%
    mutate(net_config = paste(n_cache, n_rotor, n_xpand))