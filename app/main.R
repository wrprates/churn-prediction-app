box::use(
  bslib[nav_panel, nav_spacer, page_navbar],
  shiny[busyIndicatorOptions, moduleServer, NS, useBusyIndicators],
  shiny.router[router_server],
)

box::use(
  app / logic / data_store,
  app / logic / theme,
  app / view / churn_overview,
  app / view / customer_risk,
  app / view / financial_impact,
  app / view / predictive_model,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  busy_indicator <- list(
    useBusyIndicators(),
    busyIndicatorOptions(
      spinner_type = "dots2",
      spinner_color = "#000",
      spinner_size = "2rem",
      spinner_delay = "300ms",
      fade_opacity = 0.5,
      pulse_background = "linear-gradient(45deg, #4a57a6, #4192b5)",
      pulse_height = "3px",
      pulse_speed = "1s"
    )
  )

  page_navbar(
    title = "Customer Churn Analysis",
    id = ns("navbar"),
    header = busy_indicator,
    nav_panel("Overview", churn_overview$ui(ns("overview"))),
    nav_panel("Predictive Model", predictive_model$ui(ns("model"))),
    nav_panel("Risk Analysis", customer_risk$ui(ns("risk"))),
    nav_panel("Financial Impact", financial_impact$ui(ns("financial"))),
    theme = theme$app_theme,
    navbar_options = theme$app_navbar_options
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()

    # Initialize data store once in the main app
    message("Initializing data store in main app...")
    # Use the exported data_store instance's get_data method
    data_store$data_store$get_data()
    message("Data store initialized successfully!")

    # Initialize modules
    churn_overview$server("overview")
    predictive_model$server("model")
    customer_risk$server("risk")
    financial_impact$server("financial")
  })
}
