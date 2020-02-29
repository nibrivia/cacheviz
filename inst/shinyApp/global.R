library(tidyverse)
library(shiny)
enableBookmarking(store = "url")

load_experiments <- function() {
  tibble(filename = list.files(path = "~/rotorsim/data",
                               pattern = "headers-.*.csv",
                               full.names = TRUE)) %>%
    mutate(metadata = map(filename, read_csv, n_max = 1, col_types = cols())) %>%
    unnest(metadata) %>%
    mutate(done  = grepl("done",  filename),
           drain = grepl("drain", filename),
           time  = lubridate::as_datetime(timestamp, tz = Sys.timezone()),
           net_config = paste(n_cache, n_rotor, n_xpand),
           type = ifelse(n_rotor == 0, "xpand",
                         ifelse(n_rotor == n_cache, "cache", "opera*")))
}

experiments <- load_experiments()
