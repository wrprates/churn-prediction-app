box::use(
  bsicons[bs_icon],
  bslib[card, card_header, layout_column_wrap, page_fluid, value_box],
  dplyr[`%>%`, filter, group_by, pull, summarise],
  highcharter[hc_title, hc_xAxis, hc_yAxis,
              hcaes, hchart, highchartOutput, JS, renderHighchart],
  shiny[moduleServer, NS, renderText, textOutput],
)

box::use(
  app/logic/data_processing,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    layout_column_wrap(
      width = 1 / 3,
      value_box(
        title = "Total Monthly Revenue",
        value = textOutput(ns("total_revenue")),
        showcase = bs_icon("currency-dollar"),
        theme = "success"
      ),
      value_box(
        title = "At Risk Revenue",
        value = textOutput(ns("risk_revenue")),
        showcase = bs_icon("exclamation-triangle"),
        theme = "warning"
      ),
      value_box(
        title = "Average Customer Value",
        value = textOutput(ns("avg_customer_value")),
        showcase = bs_icon("person-fill"),
        theme = "info"
      )
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Financial Impact by Risk Group"),
        highchartOutput(ns("charge_risk_groups"))
      )
    ),
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Revenue Distribution"),
        highchartOutput(ns("revenue_distribution"))
      ),
      card(
        card_header("Monthly Charges Trend"),
        highchartOutput(ns("monthly_trend"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data_processing$initialize_data()
    # Calculate financial metrics
    output$total_revenue <- renderText({
      total <- sum(data$raw_data$MonthlyCharges, na.rm = TRUE)
      paste0("$", format(total, big.mark = ",", scientific = FALSE))
    })
    output$risk_revenue <- renderText({
      high_risk <- data$predictions %>%
        filter(RiskGroup <= 3) %>%
        summarise(total = sum(MonthlyCharges, na.rm = TRUE)) %>%
        pull(total)
      paste0("$", format(high_risk, big.mark = ",", scientific = FALSE))
    })
    output$avg_customer_value <- renderText({
      avg <- mean(data$raw_data$MonthlyCharges, na.rm = TRUE)
      paste0("$", format(round(avg, 2), big.mark = ",", scientific = FALSE))
    })
    # Financial impact chart
    output$charge_risk_groups <- renderHighchart({
      data$charge_for_risk_groups %>%
        hchart(
          hcaes(x = RiskGroup, y = SumMonthlyCharges, group = Churn),
          type = "column"
        ) %>%
        renderHighchart::hc_plotOptions(column = list(
          stacking = "normal",
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
              function() {
                if(this.y === 0) return null;
                return '$' + Math.round(this.y / 1000) + 'k';
              }
            ")
          )
        )) %>%
        hc_yAxis(
          title = list(text = "Monthly Charges ($)")
        ) %>%
        hc_xAxis(
          title = list(text = "Risk Group")
        ) %>%
        hc_title(
          text = "Monthly Charges by Risk Group"
        )
    })
    # Revenue distribution chart
    output$revenue_distribution <- renderHighchart({
      data$raw_data %>%
        group_by(Contract) %>%
        summarise(
          TotalRevenue = sum(MonthlyCharges, na.rm = TRUE)
        ) %>%
        hchart(
          type = "pie",
          hcaes(x = Contract, y = TotalRevenue)
        ) %>%
        hc_title(
          text = "Revenue by Contract Type"
        )
    })
    # Monthly trend chart
    output$monthly_trend <- renderHighchart({
      data$raw_data %>%
        group_by(Contract) %>%
        summarise(
          AvgCharges = mean(MonthlyCharges, na.rm = TRUE)
        ) %>%
        hchart(
          type = "column",
          hcaes(x = Contract, y = AvgCharges)
        ) %>%
        hc_title(
          text = "Average Monthly Charges by Contract"
        ) %>%
        hc_yAxis(
          title = list(text = "Average Monthly Charges ($)")
        )
    })
  })
}
