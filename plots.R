library(tidyverse)
library(reshape2)
library(hrbrthemes)
library(scales)

queue_count_compute <- function(experiments) {
    experiments %>%
        mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map(filename, function(fn) {
            print(fn)
            res <- cache(paste0(fn, ".queue_size.cache"), {
                fread_cache(fn, skip = "flow_id") %>%
                    mutate(end = ifelse(is.nan(end),
                                        max(end, na.rm = TRUE),
                                        end)) %>%
                    select(start, end, sent) %>%
                    melt(id.vars = c("sent"), variable.name = "event", value.name = "time") %>%
                    filter(time < max(time)) %>%
                    mutate(start = (event == "start")) %>%
                    arrange(time) %>%
                    mutate(flows = cumsum(ifelse(start, 1, -1)),
                           bytes = cumsum(as.numeric(ifelse(start, sent, -sent)))) %>%
                    sample_n(10000)
            })
            incProgress(1/nrow(experiments), detail = fn)
            return(res)
        })) %>%
        rename(date = time) %>%
        unnest(arrivals) %>%
        ungroup()
}

byte_count <- function(experiments) {
    labels <- gen_labels(experiments) %>% select(uuid, label)
    q <- queue_count_compute(experiments)

    q %>%
        left_join(labels, by = "uuid") %>%
        ggplot(aes(x = time,
                   y = bytes,
                   color = label,
                   group = uuid)) +
        geom_line() +
        theme_ipsum_rc() +
        labs(color = NULL)
}
flow_count <- function(experiments) {
    labels <- gen_labels(experiments) %>% select(uuid, label)
    q <- queue_count_compute(experiments)

    q %>%
        left_join(labels, by = "uuid") %>%
        ggplot(aes(x = time,
                   y = flows,
                   color = label,
                   group = uuid)) +
        geom_line() +
        theme_ipsum_rc() +
        labs(color = NULL)
}

opera_cmp_plot <- function(experiments) {
    fcts <- experiments %>%
        gen_labels() %>%
        mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map2(filename, n_switches, function(fn, n_switches) {
            print(fn)
            print(n_switches)
            res <- cache(paste0(fn, ".fct99_cdf.cache"), {
                fread_cache(fn, skip = "flow_id", reload = TRUE) %>%
                    mutate(src_tor = floor(src/n_switches),
                           dst_tor = floor(dst/n_switches)) %>%
                    filter(!is.na(fct),
                           src_tor != dst_tor
                           ) %>%
                    #   filter(load == .01) %>%
                     group_by(size) %>%
                        #mutate(cdf = percent_rank(fct)) %>%
                          summarize(fct99 = quantile(fct, .99),
                                    fct01 = quantile(fct, .01),
                                    fct50 = quantile(fct, .50),
                                    fct   = mean(fct),
                                    #size = size,
                                    n     = n()) %>%
                         ungroup()
            }, T)
            #incProgress(1/nrow(experiments), detail = fn)
            return(res)
        })) %>%
        unnest(arrivals) %>%
        ungroup() %>%
        mutate(#fct = ifelse(load <= .01, fct, fct99),
               fct = fct99,
               load_name = paste(label, "99%-ile")) %>%
        rename(exp_id = uuid)


    lower_limit <- tibble(size_b = 10^(seq(2, 9, length.out = 100)),
                          delay   = (size_b*8/10e9+500e-9)*1e6)


    opera_colors <-
        tribble(
            ~load, ~color,   ~shape,
            .01,   "blue",   1,
            .10,   "red",    3,
            .25,   "orange", 4,
            .30,   "purple", 5,
            .40,   "green",  8
        )

    fcts %>%
        #left_join(opera_colors) %>%
        mutate(load = as_factor(load*100)) %>%

        #sample_n(10000) %>%
        #filter(load == .01) %>%
        ggplot(aes(x = size/8,
                   y = fct*1e3,
                   color = load_name,
                   shape = load_name,
                   linetype = load_name,
                   group = exp_id)) +
        geom_line(show.legend = T) +
        #geom_text(aes(label = n), nudge_y=.5, angle = 45) +
        geom_point() +
        geom_line(data = lower_limit, aes(x=size_b, y = delay), linetype = "dashed",
                  inherit.aes = F) +
        geom_vline(xintercept = 12500*1500, color = "red", linetype = "dashed") +
        geom_vline(xintercept =   888*1500, color = "red", linetype = "dashed") +
        scale_x_log10(breaks = 10^(-10:10),
                      label = label_math(format = log10)) +
        scale_y_log10(breaks = 10^((-5:5)*2), label = label_math(format = log10)) +
        theme_ipsum_rc() +
        #scale_color_manual(values = opera_colors$color) +
        #scale_shape_manual(values = opera_colors$shape) +

        theme(legend.position = c(.85, .3)) +
        labs(title = "Flow completion times (opera-style)",
             color = NULL, shape = NULL, linetype = NULL,
             x = "Flow size (bytes)",
             y = "Flow completion time (μs)",
             caption = "github.com/nibrivia/rotorsim")
}

tput_plot <- function(tput_experiments) {
    require(data.table)
    require(scales)
    require(hrbrthemes)
    tputs <- tput_experiments %>%
        mutate(type = ifelse(n_rotor == 0, "xpand", ifelse(n_rotor == 8, "cache", "opera*")),
               filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map(filename,
                              function(fn) {
                                  res <- cache(paste0(fn, ".tput.cache"), {
                                      fread_cache(fn, skip = "flow_id") %>%
                                          group_by(src) %>%
                                          summarize(gbits_sent  = sum(sent/1e9),
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
                   color = paste(net_config, cache_policy),
                   shape = paste(net_config, cache_policy),
                   #color = done,
                   group = paste(net_config, cache_policy))) +
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

