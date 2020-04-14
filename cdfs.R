cdf_plot <- function(exps) {
  cdfs <- exps %>%
    gen_labels() %>%
    mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
    mutate(arrivals = map(filename,
                          function(fn) {
                            print(fn)
                            fread_cache(fn, skip = "flow_id") %>%
                              filter(!is.na(fct)) %>%
                              mutate(p = round(percent_rank(fct), 3)) %>%
                              group_by(p) %>%
                                summarize(fct = max(fct)) %>%
                                ungroup()
                          })) %>%
    unnest(arrivals)

  cdfs %>%
      ggplot(aes(x = fct,
                 y = p,
                 color = label,
                 group = uuid)) +
      geom_step() +
      scale_y_percent() +
      scale_x_log10(breaks = 10^(-3:4),
                    labels = c("1us", "10", "100", "1ms", "10", "100", "1s", "10s")) +
      theme_ipsum_rc() +
      theme(legend.position = c(.9, .4)) +
      labs(x = "FCT", y = "CDF",
           color = NULL,
           title = "Flow completion times CDF",
           caption = "https://github.com/nibrivia/rotorsim")
}


cdf_plot_size <- function(exps) {
  cdfs <- exps %>%
      gen_labels() %>%
      mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
      mutate(arrivals = map(filename,
                            function(fn) {
                                print(fn)
                                fread_cache(fn, skip = "flow_id", reload = TRUE) %>%
                                    filter(!is.na(fct)) %>%
                                    mutate(size = as.numeric(size)) %>%
                                    group_by(size) %>%
                                        mutate(p = round(percent_rank(fct), 3)) %>%
                                    group_by(p, size) %>%
                                        summarize(fct = max(fct)) %>%
                                        ungroup()
                            })) %>%
      unnest(arrivals)

  cdfs %>%
      ggplot(aes(x = fct,
                 y = p,
                 color = label,
                 group = uuid)) +
      geom_step() +
      scale_y_percent() +
      scale_x_log10(breaks = 10^(-3:4),
                    labels = c("1us", "10", "100", "1ms", "10", "100", "1s", "10s")) +
      facet_wrap(~size, scales = "free_x") +
      theme_ipsum_rc() +
      labs(x = "FCT", y = "CDF",
           color = NULL,
           title = "Flow completion times CDF by size (bits)",
           caption = "https://github.com/nibrivia/rotorsim")
}

function() {
    exps <- experiments %>%
        filter(time_limit == 10000,
               is_ml,
               n_tor == 33,
               load == .8,
               workload == "datamining"
               )
    print(exps)
    print(nrow(exps))

    flows <- exps %>%
        gen_labels() %>%
        mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map(filename, fread_cache, skip = "flow_id")) %>%
        unnest(arrivals) %>%
        mutate(size = as.numeric(size)) %>%
        filter(!is.na(fct))

    flows %>%
        group_by(uuid, label, size) %>%
            summarize(n=n()) %>%
            ungroup() %>%
        ggplot(aes(x = size,
                   y = n,
                   fill = label,
                   color = label)) +
        geom_line()  +
        geom_point()  +
        scale_x_log10() +
        scale_y_log10()
}