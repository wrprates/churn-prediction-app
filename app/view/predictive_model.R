box::use(
  bslib[card, card_header, layout_column_wrap],
  highcharter[hc_chart, hc_colors, hc_legend, hc_size, hc_title, hc_tooltip,
              hc_xAxis, hc_yAxis],
  shiny[div, h1, moduleServer, NS],
)

box::use(
  app/logic/data_processing,
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
        highcharter::highchartOutput(ns("vars_importance"), height = "500px")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data_processing$initialize_data()
    output$vars_importance <- highcharter::renderHighchart({
      highchart::highchart() |>
        highchart::hc_add_series(data$vars$importance$percentage * 100, name = "") |>
        hc_chart(type = "bar", zoomType = "xy") |>
        hc_xAxis(categories = data$vars$importance$variable) |>
        hc_yAxis(
          title = list(text = "Importance Percentage"),
          labels = list(format = "{value}%")
        ) |>
        hc_colors("#4192b5") |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(
          formatter = highchart::JS(
            "function(){return 'Importance (%): <b>' + Highcharts.numberFormat(this.y) + '%</b>';}"
          ),
          useHTML = FALSE
        ) |>
        hc_title(text = "Variables Importance") |>
        hc_size(width = NULL, height = 500)
    })
  })
}
