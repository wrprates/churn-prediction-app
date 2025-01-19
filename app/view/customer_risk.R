box::use(
  bslib[card, card_header, layout_column_wrap, page_fluid],
  dplyr[`%>%`, filter],
  highcharter[hc_add_series, hc_xAxis, hc_yAxis, hcaes,
              hchart, highchartOutput, renderHighchart],
  reactable[reactableOutput, renderReactable],
  shiny[moduleServer, NS],
)

box::use(
  app/logic/data_processing,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    layout_column_wrap(
      width = 1,
      card(
        card_header("Customer Risk Distribution"),
        highchartOutput(ns("risk_groups_churn"))
      )
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("High Risk Customers"),
        reactableOutput(ns("risk_table"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data_processing$initialize_data()
    output$risk_groups_churn <- renderHighchart({
      data$churn_by_risk_groups |>
        hchart(
          hcaes(x = RiskGroup, y = prop, group = Churn),
          type = "column"
        ) |>
        hc_add_series(
          name = "Cumulative % of canceled customers (recall)",
          data = (data$churn_by_risk_groups %>%
                    filter(Churn == "Yes"))$cum_prop,
          type = "line",
          dashStyle = "DashDot"
        ) |>
        hc_yAxis(
          title = list(text = "Proportion (%)"),
          max = 100
        ) |>
        hc_xAxis(
          title = list(text = "Risk Group")
        )
    })
    output$risk_table <- renderReactable({
      data$predictions |>
        filter(RiskGroup <= 3) |>  # Show top 3 risk groups
        reactable::reactable(
          columns = list(
            Contract = reactable::colDef(minWidth = 150),
            PaymentMethod = reactable::colDef(minWidth = 150)
          ),
          highlight = TRUE,
          striped = FALSE,
          filterable = TRUE,
          searchable = TRUE,
          compact = TRUE,
          defaultPageSize = 10
        )
    })
  })
}
