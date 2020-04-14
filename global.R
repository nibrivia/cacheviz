library(tidyverse)
library(shiny)
source("helpers.R")
source("cdfs.R")
source("drain.R")
source("plots.R")
enableBookmarking(store = "url")

load_experiments <- function() {
    exps <- tibble(filename = list.files(path = "~/rotorsim/data",
                                         pattern = "headers-.*.csv",
                                         full.names = TRUE)) %>%
        mutate(metadata = map(filename, read_csv, n_max = 1, col_types = cols()),
               nrow     = map(metadata, nrow)) %>%
        filter(nrow > 0)

    bind_rows(exps$metadata) %>%
        bind_cols(exps %>% select(filename)) %>%
        mutate(done  = grepl("done",  filename),
               drain = grepl("drain", filename),
               time  = lubridate::as_datetime(timestamp, tz = "America/New_York"),
               net_config = paste(n_cache, n_rotor, n_xpand),
               type = ifelse(n_rotor == 0, "xpand",
                             ifelse(n_rotor == n_cache, "cache", "opera*")))
}

experiments <- load_experiments()

function() {
    count <- experiments %>%
        filter(time_limit > 100) %>%
        mutate(sim_n = rank(time))
    count %>%
        ggplot(aes(x = time,
                   y = sim_n)) +
        geom_hline(aes(yintercept = 300,
                       color = "# SIGCOMM simulations")) +
        geom_step() +
        scale_y_comma() +
        labs(x = NULL, y = "# simulations",
             color = NULL,
             caption = "https://github.com/nibrivia/cachebot") +
        theme_ipsum_rc() +
        theme(legend.position = c(.9, .1))
}

plot_fns <- tribble(
    ~name,                        ~key,         ~fn,           ~enable,
    'Opera-style FCTs',          "fct_opera",  opera_cmp_plot, TRUE,
    'Demand Completion (drain)', "dct",        dct_plot,       TRUE,
    'Tput by ToR (skew)',        "tput_tor",   tput_plot,      TRUE,
    'FCT CDF (all flows)',       "fct_flow",   cdf_plot,       TRUE,
    'FCT CDF (by size)',         "fct_size",   cdf_plot_size,  TRUE,

)

plot_choices <- plot_fns$key
names(plot_choices) <- plot_fns$name
