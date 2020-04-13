library(shiny)
library(shinydashboard)


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
            select(uuid, n_tor, time_limit, load,
                   cache_rotor_xpand = net_config, workload,
                   cache_policy,
                   drain = arrive_at_start, skewed, is_ml, valiant,
                   date = time)
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
        keep_valiant <- "valiant" %in% input$flags
        ret <- ret %>%
            filter(is_ml == keep_ml,
                   skewed == keep_skew,
                   valiant == keep_valiant | is.na(valiant),
                   drain == keep_drain)

        ret <- ret %>%
            group_by(net_config, cache_policy, n_switches, n_tor, load, time_limit, workload) %>%
                top_n(1, time) %>%
                ungroup()


        ret %>% arrange(n_tor, time_limit, net_config, load, workload)
    })

    size_gb_csv <- reactive({
        s <- 0
        invalidateLater(1000)
        fns <- paste0("~/rotorsim/data/done-", selected_exps()$uuid, ".csv.rds")
        for (fn in fns) {
            if (file.exists(fn)) {
                s <- s + file.size(fn)
            }
        }
        s/1e9
    })

    size_gb <- reactive({
        s <- 0
        invalidateLater(1000)
        cache_suffix <- ".rds"
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
            value    = paste0(round(size_gb(),     1), "GB cache"),
            subtitle = paste0(round(size_gb_csv(), 1), "GB data"),
            color = color_size(),
            icon = icon("database")
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

        row <- plot_fns %>% filter(key == input$plot_type)
        plot_fn <- row[["fn"]][[1]]

        withProgress(message = "Reading data...", value = 0, {
            plot_fn(selected_exps())
        })
    })

    output$plot <- renderPlot({re_plot()})
}

