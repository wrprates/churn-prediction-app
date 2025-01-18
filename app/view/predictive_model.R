box::use(
  highcharter[highchartOutput, renderHighchart, hchart, hc_add_series, hc_chart,
              hc_xAxis, hc_yAxis, hc_colors, hc_legend, hc_tooltip, hc_title, 
              hc_size, JS],
  shiny[div, h1, h2, moduleServer, NS],
  bslib[layout_column_wrap, card, card_header],
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
        highchartOutput(ns("vars_importance"), height = "500px")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data_processing$initialize_data()
    
    output$vars_importance <- renderHighchart({
      highcharter::highchart() |>
        hc_add_series(data$vars$importance$percentage * 100, name = "") |>
        hc_chart(type = "bar", zoomType = "xy") |>
        hc_xAxis(categories = data$vars$importance$variable) |>
        hc_yAxis(
          title = list(text = "Importance Percentage"), 
          labels = list(format = "{value}%")
        ) |>
        hc_colors("#4192b5") |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(
          formatter = JS(
            "function(){return 'Importance (%): <b>' + Highcharts.numberFormat(this.y) + '%</b>';}"
          ),
          useHTML = FALSE
        ) |>
        hc_title(text = "Variables Importance") |>
        hc_size(width = NULL, height = 500)
    })
  })
}
