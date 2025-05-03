box::use(
  bslib[nav_panel, nav_spacer, page_navbar, navbar_options],
  shiny.router[router_server],
  shiny[busyIndicatorOptions,
        moduleServer,
        NS,
        useBusyIndicators],
)

box::use(
  app/view/churn_overview,
  app/view/customer_risk,
  app/view/financial_impact,
  app/view/predictive_model,
  app/logic/theme,
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
    bg = "#f8f9fa"
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
