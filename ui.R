library(shiny)
library(shinydashboard)
library(DT)

workloads <- unique(experiments$workload)
time_limits <- sort(unique(experiments$time_limit))
n_tor <- unique(experiments$n_tor)
n_switches <- sort(unique(experiments$n_switches))
loads <- sort(unique(experiments$load))
sw_configs <- experiments %>%
    arrange(n_cache, n_rotor, n_xpand) %>%
    .$net_config %>% unique()

function(request) {
    dashboardPage(
        skin = "green",
        dashboardHeader(title = "Cacheplot"),
        dashboardSidebar(
            bookmarkButton(),
            selectInput("plot_type", "Plot",
                        choices = c(
                            `Throughput by ToR` = "tput_tor",
                            `Opera-style FCTs`  = "fct_opera"
                            #`FCT`         = "fct",
                            #`FCT by size` = "fct_size",
                            #None     = "none"
                                    )),
            actionButton("plot_button", "Plot it!", icon = icon("chart-line")),
            br(),
            selectizeInput("n_tor", "# racks",
                           choices = n_tor,
                           multiple = TRUE),
            selectizeInput("n_switches", "# switches",
                           choices = n_switches,
                           multiple = TRUE),
            selectizeInput("net_config", "cache/rotor/xpand",
                           choices = sw_configs,
                           multiple = TRUE),


            br(),
            selectizeInput("time_limit", "Duration (ms)",
                           choices = time_limits, multiple = FALSE,
                           selected = max(time_limits)),
            checkboxGroupInput("workloads", "Workloads",
                               choices = workloads,
                               selected = workloads),
            selectizeInput("loads", "Load",
                           choices = loads, multiple = TRUE,
                           selected = loads),

            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            menuItem("simulator", icon = icon("github"),
                     href = "https://github.com/nibrivia/rotorsim"),
            menuItem("cachebot", icon = icon("github"),
                     href = "https://github.com/nibrivia/cachebot"),
            menuItem("cacheviz", icon = icon("github"),
                     href = "https://github.com/nibrivia/cacheviz")
        ),
        dashboardBody(
            # Boxes need to be put in a row (or column)
            fluidRow(
                valueBoxOutput("n_select_box",  width = 4),
                valueBoxOutput("data_size_box", width = 4),
                valueBoxOutput("note_box", width = 4)

            ),
            fluidRow(
                box(title = "Selected experiments",
                    collapsible = TRUE,
                    width = 12,
                    DT::dataTableOutput("experiments")
                )
            ),
            fluidRow(
                box(title = "Plot",
                    width = 12,
                    plotOutput("plot", height = 450)
                )
            )
        )
    )
}
