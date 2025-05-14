box::use(
  bslib[card, card_header, layout_column_wrap],
  highcharter[
    hc_add_series,
    hc_chart,
    hc_colors,
    hc_legend,
    hc_size,
    hc_title,
    hc_tooltip,
    hc_xAxis,
    hc_yAxis,
    highchartOutput,
    renderHighchart,
    highchart,
    JS
  ],
  shiny[div, h1, moduleServer, NS],
)

box::use(
  app / logic / data_store
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    h1("Predictive Model"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Variables Importance"),
        highchartOutput(ns("vars_importance"), height = "500px")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Get data from shared data store
    message("Predictive Model: Getting data from data_store")
    data <- data_store$data_store$get_data()

    message("Predictive Model module using shared data")

    output$vars_importance <- renderHighchart({
      highchart() |>
        hc_add_series(data$vars$importance$percentage * 100, name = "") |>
        hc_chart(type = "bar", zoomType = "xy") |>
        hc_xAxis(categories = data$vars$importance$variable) |>
        hc_yAxis(
          title = list(text = "Importance Percentage"),
          labels = list(format = "{value}%"),
          max = 50
        ) |>
        hc_colors("#4192b5") |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(
          formatter = JS(
            "function(){return 'Importance (%): <b>' + Highcharts.numberFormat(this.y, 2) + '%</b>';}"
          ),
          useHTML = FALSE
        ) |>
        hc_title(text = "Variables Importance") |>
        hc_size(width = NULL, height = 500)
    })
  })
}
