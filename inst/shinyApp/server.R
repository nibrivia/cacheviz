library(shiny)
library(shinydashboard)
library(tidyverse)

cache <- function(id, computation, reload = FALSE) {
    if (reload | !file.exists(id)) {
        res <- computation
        saveRDS(object = res, file = id)
        return(res)
    } else {
        readRDS(file = id)
    }
}

fread_cache <- function(input, ...) {
    input_cache <- paste0(input, ".rds")
    cache(input_cache, fread(input, ...))
}

opera_cmp_plot <- function(experiments) {
    fcts <- experiments %>%
        mutate(filename = paste0("~/rotorsim/data/done-", uuid, ".csv")) %>%
        mutate(arrivals = map(filename, function(fn) {
            print(fn)
            res <- cache(paste0(fn, ".fct99_cdf.cache"), {
                fread_cache(fn, skip = "flow_id") %>%
                    filter(!is.na(fct)) %>%
                    #filter(load == .01) %>%
                    group_by(size) %>%
                    summarize(fct99 = quantile(fct, .99),
                              fct   = mean(fct),
                              n     = n()) %>%
                    ungroup()
            })
            incProgress(1/nrow(experiments), detail = fn)
            return(res)
        })) %>%
        unnest(arrivals) %>%
        ungroup() %>%
        mutate(fct = ifelse(load <= .01, fct, fct99),
               load_name = paste0(100*load, "% load, ", ifelse(load == 0.01, "avg", "99%-ile"))) %>%
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
                   color = paste(load_name, net_config),
                   shape = paste(load_name, net_config),
                   linetype = paste(load_name, net_config),
                   group = exp_id)) +
        geom_line(show.legend = T) +
        #geom_text(aes(label = n), nudge_y=.5, angle = 45) +
        geom_point() +
        geom_line(data = lower_limit, aes(x=size_b, y = delay), linetype = "dashed",
                  inherit.aes = F) +
        geom_vline( xintercept = 15e6, color = "red", linetype = "dashed") +
        scale_x_log10(breaks = 10^(-10:10), label = label_math(format = log10)) +
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
                   color = net_config,
                   shape = net_config,
                   #color = done,
                   group = net_config)) +
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    experiments <- reactive({
        if (is.null(input$refresh_experiments)) {
            cache("experiments", {load_experiments()})
        } else {
            cache("experiments", {load_experiments()}, input$refresh_experiments == 1)
        }
    })

    output$experiments_table <- DT::renderDataTable({
        selected_exps() %>%
            select(n_tor, time_limit, load, cache_rotor_xpand = net_config, workload,
                   drain = arrive_at_start, skewed, is_ml)
    },
    options = list(pageLength = 15, dom = "ftp"),
    rownames = FALSE)

    selected_exps <- reactive({
        ret <- experiments() %>%
            filter(time_limit == input$time_limit) %>%
            filter(workload %in% input$workloads)
        if (length(input$net_config) != 0)
            ret <- ret %>% filter(net_config %in% input$net_config)
        if (length(input$n_switches) != 0)
            ret <- ret %>% filter(n_switches %in% input$n_switches)
        if (length(input$n_tor) != 0)
            ret <- ret %>% filter(n_tor %in% input$n_tor)
        if (length(input$loads) != 0)
            ret <- ret %>% filter(load %in% input$loads)

        keep_ml    <- "is_ml" %in% input$flags
        keep_skew  <- "skewed" %in% input$flags
        keep_drain <- "arrive_at_start" %in% input$flags
        ret <- ret %>%
            filter(is_ml == keep_ml,
                   skewed == keep_skew,
                   drain == keep_drain)

        ret <- ret %>%
            group_by(net_config, n_switches, n_tor, load, time_limit, workload) %>%
                top_n(1, time) %>%
                ungroup()


        ret %>% arrange(n_tor, time_limit, net_config, load, workload)
    })

    size_gb <- reactive({
        s <- 0
        invalidateLater(1000)
        input$plot_type
        if (input$plot_type == "fct_opera") {
            cache_suffix <- ".fct99_cdf.cache"
        }
        if (input$plot_type == "tput_tor") {
            cache_suffix <- ".tput.cache"
        }
        fns <- paste0("~/rotorsim/data/done-", selected_exps()$uuid, ".csv")
        for (fn in fns) {
            rds_fn <- paste0(fn, ".rds")
            cache_fn <- paste0(fn, cache_suffix)
            if (file.exists(cache_fn)) {
                s <- s + file.size(cache_fn)
            } else if(file.exists(rds_fn)) {
                s <- s + file.size(rds_fn)
            } else {
                s <- s + file.size(fn)
            }
        }
        s/1e9
    })

    color_size <- reactive({
        color <- "green"
        if (is.na(size_gb())) {
            return("aqua")
        }

        if (size_gb() > 2) {
            color <- "orange"
        }
        if (size_gb() > 10) {
            color <- "red"
        }
        return(color)
    })
    output$data_size_box <- renderValueBox({

        valueBox(
            value = paste(round(size_gb(), 1), "GB"),
            color = color_size(),
            icon = icon("database"),
            subtitle = "of csv/cache for this plot"
        )
    })

    output$n_select_box <- renderValueBox({
        valueBox(
            icon = icon("th-list"),
            value = span(nrow(selected_exps()), "sims",
                          actionButton("refresh_experiments", "refresh")),

            subtitle = span(nrow(experiments()), "total")
        )
    })

    output$note_box <- renderValueBox({
        icon <- NULL
        if (color_size() == "orange")
            icon <- NULL
        if (color_size() == "red")
            icon <- icon("grin-beam-sweat")

        valueBox("Enjoy :)",
                 icon = icon,
                 subtitle = img(href = "https://plotypus.csail.mit.edu/plotypus.jpg"),
                 width = 4)
    })

    re_plot <- eventReactive(input$plot_button, {
        if (size_gb() > 10) {
            return(NULL)
        }

        withProgress(message = "Reading .csvs", value = 0, {
            if (input$plot_type == "tput_tor") {
                return(tput_plot(selected_exps()))
            }
            if (input$plot_type == "fct_opera") {
                return(opera_cmp_plot(selected_exps()))
            }
        })
    })

    output$plot <- renderPlot({
        re_plot()
    })
}

