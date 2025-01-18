box::use(
  bslib[nav_panel, nav_spacer, page_navbar],
  shiny[busyIndicatorOptions,
        moduleServer,
        NS,
        useBusyIndicators],
  shiny.router[router_server],
)

box::use(
  app/view/churn_overview,
  app/view/customer_risk,
  app/view/financial_impact,
  app/view/predictive_model,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "Customer Churn Analysis",
    header = list(
      useBusyIndicators(),
      busyIndicatorOptions(
        spinner_type = "dots2",
        spinner_color = "#4a57a6",  # Using primary color
        spinner_size = "2rem",
        spinner_delay = "300ms",
        fade_opacity = 0.5,
        pulse_background = "linear-gradient(45deg, #4a57a6, #4192b5)",
        pulse_height = "3px",
        pulse_speed = "1s"
      )
    ),
    nav_panel("Overview", churn_overview$ui(ns("overview"))),
    nav_panel("Predictive Model", predictive_model$ui(ns("model"))),
    nav_panel("Risk Analysis", customer_risk$ui(ns("risk"))),
    nav_panel("Financial Impact", financial_impact$ui(ns("financial"))),
    nav_spacer(),
    bg = "#f8f9fa",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "lux",
      primary = "#4a57a6",
      secondary = "#4192b5",
      success = "#28a745",
      info = "#17a2b8",
      warning = "#ffc107",
      danger = "#dc3545"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server()
    # Initialize modules
    churn_overview$server("overview")
    predictive_model$server("model")
    customer_risk$server("risk")
    financial_impact$server("financial")
  })
}
