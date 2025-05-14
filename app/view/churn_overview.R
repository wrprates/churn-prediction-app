box::use(
  bsicons[bs_icon],
  bslib[card, card_header, layout_column_wrap, page_fluid, value_box],
  dplyr[group_by, summarise],
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
    hcaes,
    hchart,
    highchartOutput,
    renderHighchart,
    highchart,
    JS
  ],
  shiny[div, moduleServer, NS, p, renderText, tags, textOutput],
  utils[head]
)

box::use(
  app / logic / data_store
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    # Main Metrics
    layout_column_wrap(
      width = 1 / 3,
      value_box(
        title = "Total Customers",
        value = textOutput(ns("total_customers")),
        showcase = bs_icon("people-fill"),
        p("Total number of customers in the database"),
        theme = "primary"
      ),
      value_box(
        title = "Churn Rate",
        value = textOutput(ns("churn_rate")),
        showcase = bs_icon("graph-up"),
        p("Percentage of customers who have left the company"),
        theme = "warning"
      ),
      value_box(
        title = "Monthly Revenue",
        value = textOutput(ns("monthly_revenue")),
        showcase = bs_icon("currency-dollar"),
        p("Total monthly charges from all customers"),
        theme = "success"
      )
    ),
    # Overall Churn Distribution
    card(
      card_header("Overall Churn Distribution"),
      div(
        class = "p-3",
        p(
          "This chart shows the overall distribution of churned vs non-churned customers. 
          It provides a quick overview of the company's customer retention situation."
        ),
        highchartOutput(ns("overall_churn"))
      )
    ),
    # Risk Factors and Monthly Trends
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Key Risk Factors"),
        div(
          class = "p-3",
          p(
            "This chart displays the most important variables that influence customer churn, 
            ranked by their impact on the model's predictions. Understanding these factors 
            helps in developing targeted retention strategies."
          ),
          highchartOutput(ns("risk_factors"))
        )
      ),
      card(
        card_header("Contract Type Analysis"),
        div(
          class = "p-3",
          p(
            "This visualization shows the relationship between contract types and churn rates. 
            It helps identify which contract arrangements are associated with higher customer
            retention."
          ),
          highchartOutput(ns("monthly_trends"))
        )
      )
    ),
    # Insights Section
    card(
      card_header("Key Insights"),
      div(
        class = "p-3",
        tags$ul(
          tags$li(
            "Contract type is one of the strongest predictors of customer churn"
          ),
          tags$li(
            "Month-to-month contracts show significantly higher churn rates"
          ),
          tags$li(
            "Customers with higher monthly charges are more likely to churn"
          ),
          tags$li(
            "Technical support availability significantly impacts customer retention"
          )
        )
      )
    )
  )
}

# Helper functions for data processing
get_total_customers <- function(data) {
  if (!is.data.frame(data$raw_data)) return(0)
  nrow(data$raw_data)
}

get_churn_rate <- function(data) {
  if (
    !is.data.frame(data$raw_data) ||
      nrow(data$raw_data) == 0 ||
      !"Churn" %in% names(data$raw_data)
  ) {
    return("0.0%")
  }
  paste0(round(mean(data$raw_data$Churn == "Yes", na.rm = TRUE) * 100, 1), "%")
}

get_monthly_revenue <- function(data) {
  if (
    !is.data.frame(data$raw_data) ||
      nrow(data$raw_data) == 0 ||
      !"MonthlyCharges" %in% names(data$raw_data)
  ) {
    return("$0.00")
  }
  paste0(
    "$",
    format(
      sum(data$raw_data$MonthlyCharges, na.rm = TRUE),
      big.mark = ",",
      scientific = FALSE
    )
  )
}

# Helper functions for charts
create_overall_churn_chart <- function(data) {
  if (!is.data.frame(data$overall_churn) || nrow(data$overall_churn) == 0) {
    return(
      highchart() |>
        hc_title(text = "No churn data available") |>
        hc_subtitle(text = "Please check your data source")
    )
  }

  data$overall_churn |>
    hchart(
      hcaes(x = Customer, y = `% Customers`, group = Churn),
      type = "bar",
      stacking = "normal",
      dataLabels = list(enabled = TRUE)
    ) |>
    hc_title(text = "Overall company's Churn") |>
    hc_xAxis(title = list(text = "")) |>
    hc_yAxis(max = 100) |>
    hc_tooltip(
      formatter = JS(
        "function() { return this.series.name + ': <b>' + Highcharts.numberFormat(this.y, 2) + '%</b>'; }"
      )
    )
}

create_risk_factors_chart <- function(data) {
  if (
    !is.list(data$vars) ||
      !is.data.frame(data$vars$importance) ||
      nrow(data$vars$importance) == 0
  ) {
    return(
      highchart() |>
        hc_title(text = "No variable importance data available") |>
        hc_subtitle(text = "Please check your data source")
    )
  }

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
      )
    ) |>
    hc_title(text = "Variables Importance")
}

create_monthly_trends_chart <- function(data) {
  if (
    !is.data.frame(data$raw_data) ||
      nrow(data$raw_data) == 0 ||
      !"Contract" %in% names(data$raw_data) ||
      !"Churn" %in% names(data$raw_data)
  ) {
    return(
      highchart() |>
        hc_title(text = "No contract data available") |>
        hc_subtitle(text = "Please check your data source")
    )
  }

  data$raw_data |>
    group_by(Contract) |>
    summarise(
      AvgCharges = mean(MonthlyCharges, na.rm = TRUE),
      ChurnRate = mean(Churn == "Yes", na.rm = TRUE) * 100
    ) |>
    hchart(type = "column", hcaes(x = Contract, y = ChurnRate)) |>
    hc_title(text = "Churn Rate by Contract Type") |>
    hc_yAxis(title = list(text = "Churn Rate (%)")) |>
    hc_tooltip(
      formatter = JS(
        "function() { return 'Churn Rate: <b>' + Highcharts.numberFormat(this.y, 2) + '%</b>'; }"
      )
    )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Get data from shared data store
    message("Churn Overview: Getting data from data_store")
    data <- data_store$data_store$get_data()

    # Debug information
    if (nrow(data$raw_data) > 0) {
      message(
        "Churn Overview module using data with ",
        nrow(data$raw_data),
        " rows and first customer ID: ",
        data$raw_data$customerID[1]
      )
    }

    # Render outputs using helper functions
    output$total_customers <- renderText({
      format(get_total_customers(data), big.mark = ",")
    })

    output$churn_rate <- renderText({
      get_churn_rate(data)
    })

    output$monthly_revenue <- renderText({
      get_monthly_revenue(data)
    })

    output$overall_churn <- renderHighchart({
      create_overall_churn_chart(data)
    })

    output$risk_factors <- renderHighchart({
      create_risk_factors_chart(data)
    })

    output$monthly_trends <- renderHighchart({
      create_monthly_trends_chart(data)
    })
  })
}
