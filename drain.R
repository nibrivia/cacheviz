dct_plot <- function(exps) {
    dcts <- exps %>%
        gen_labels(ignore_cols = "load") %>%
        mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map(filename,
                              function(fn) {
                                  print(fn)
                                  fread_cache(fn, skip = "flow_id") %>%
                                      filter(!is.na(fct)) %>%
                                      summarize(max = max(fct),
                                                `99%-ile` = quantile(fct, .99)) %>%
                                      ungroup()
                              })) %>%
        unnest(arrivals) %>%
        reshape2::melt(measure.vars= c("max", "99%-ile"),
                       variable.name = "percentile",
                       value.name = "fct")

    dcts %>%
        ggplot(aes(x = load,
                   y = fct/time_limit,
                   color = label,
                   group = label)) +
        geom_point() +
        geom_line() +
        scale_x_percent() +
        #scale_x_log10(breaks = 10^(-3:4),
        #              labels = c("1us", "10", "100", "1ms", "10", "100", "1s", "10s")) +
        facet_wrap(~percentile) +
        theme_ipsum_rc() +
        theme(legend.position = c(.9, .4)) +
        labs(x = "Load", y = "DCT / duration ratio",
             color = NULL,
             title = "Demand completion times",
             caption = "https://github.com/nibrivia/rotorsim")
}

tput_plot <- function(tput_experiments) {
    tputs <- tput_experiments %>%
        mutate(type = ifelse(n_rotor == 0, "xpand", ifelse(n_rotor == 8, "cache", "opera*")),
               filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map2(filename, n_switches,
                              function(fn, n_switches) {
                                  res <- cache(paste0(fn, ".tput.cache"), {
                                      fread_cache(fn, skip = "flow_id") %>%
                                          filter(!is.na(fct)) %>%
                                          mutate(src_tor = floor(src/n_switches),
                                                 dst_tor = floor(dst/n_switches)) %>%
                                          group_by(src_tor) %>%
                                              summarize(gbits_sent  = sum(size/1e9),
                                                        max_t      = max(start, end, na.rm = TRUE),
                                                        n = n()) %>%
                                              ungroup() %>%
                                          summarize(mean_sent = mean(gbits_sent)/max(max_t/1e3),
                                                    max_time  = max(max_t),
                                                    n = sum(n))
                                  })
                                  incProgress(1/nrow(tput_experiments), detail = fn)
                                  return(res)
                              })) %>%
        unnest(arrivals)

    tputs %>%
        ggplot(aes(x = load,
                   y = mean_sent,
                   color = paste(net_config, workload),
                   shape = paste(net_config, workload),
                   #color = done,
                   group = paste(net_config, workload))) +
        geom_point() +
        geom_line() +

        scale_x_percent(   limits = c(0, 1)) +
        scale_y_continuous(limits = c(0, NA)) +

        theme_ipsum_rc() +
        labs(title = "Average ToR throughput by load",
             x = NULL,
             y = "Mean ToR tput (Gb/s)",
             color = NULL, shape = NULL,
             caption = "github.com/nibrivia/rotorsim")
}
