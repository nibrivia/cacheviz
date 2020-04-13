library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)

workloads <- unique(experiments$workload)
time_limits <- sort(unique(experiments$time_limit))
n_tor <- sort(unique(experiments$n_tor))
n_switches <- sort(unique(experiments$n_switches))
loads <- sort(unique(experiments$load))
flags <- c(`Machine Learning` = "is_ml",
           Skew            = "skewed",
           Valiant         = "valiant",
           Drain           = "arrive_at_start")
sw_configs <- experiments %>%
    arrange(n_cache, n_rotor, n_xpand) %>%
    .$net_config %>% unique()

function(request) {

    dashboardPage(
        skin = "green",
        dashboardHeader(title = "Plotypus"),
        dashboardSidebar(
            bookmarkButton(),
            selectInput("plot_type", "Plot",
                        choices = plot_choices),
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
                           selected = 2000),
            checkboxGroupInput("workloads", "Workloads",
                               choices = workloads,
                               selected = workloads),
            checkboxGroupInput("flags", NULL,
                               choices = flags,
                               selected = NULL),
            selectizeInput("loads", "Load",
                           choices = loads, multiple = TRUE,
                           selected = NULL),

            br(),
            menuItem("simulator", icon = icon("github"),
                     href = "https://github.com/nibrivia/rotorsim"),
            menuItem("cachebot", icon = icon("github"),
                     href = "https://github.com/nibrivia/cachebot"),
            menuItem("plotypus", icon = icon("github"),
                     href = "https://github.com/nibrivia/cacheviz")
        ),
        dashboardBody(
            # Boxes need to be put in a row (or column)
            fluidRow(
                valueBoxOutput("n_select_box",  width = 6),
                valueBoxOutput("data_size_box", width = 6)
#                box(div(p("This tool caches its work: start small and grow.")
                        #,downloadButton("downloadButton", "Plot data (.csv)")
                        #),
                    #width = 4)
                #valueBoxOutput("note_box", width = 4)

            )
            ,
            fluidRow(
                box(title = "Plot",
                    width = 12,
                    plotOutput("plot", height = "500px")
                )
            )
            ,
            fluidRow(
                box(id = "box_experiments",
                    title = "Selected experiments",
                    collapsible = TRUE,
                    width = 12,
                    DT::dataTableOutput("experiments_table")
                )
            )
        ),
        tags$head(tags$meta(property = "og:title",
                            content = "Plotypus"),
                  tags$title("Plotypus"),
                  tags$meta(property = "og:image",
                            content = "https://plotypus.csail.mit.edu/plotypus.jpg"))
    )
}
