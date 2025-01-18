box::use(
  shiny[moduleServer, NS, div, textOutput, renderText, p, tags],
  bslib[page_fluid, layout_column_wrap, card, card_header, value_box],
  highcharter[highchartOutput, renderHighchart, hchart, hcaes, hc_title, hc_xAxis, hc_yAxis],
  bsicons[bs_icon],
)

box::use(
  app/logic/data_processing,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    # Métricas principais
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
    
    # Distribuição geral do Churn
    card(
      card_header("Overall Churn Distribution"),
      div(
        class = "p-3",
        p("This chart shows the overall distribution of churned vs non-churned customers. 
          It provides a quick overview of the company's customer retention situation."),
        highchartOutput(ns("overall_churn"))
      )
    ),
    
    # Fatores de risco e tendências mensais
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Key Risk Factors"),
        div(
          class = "p-3",
          p("This chart displays the most important variables that influence customer churn, 
            ranked by their impact on the model's predictions. Understanding these factors 
            helps in developing targeted retention strategies."),
          highchartOutput(ns("risk_factors"))
        )
      ),
      card(
        card_header("Contract Type Analysis"),
        div(
          class = "p-3",
          p("This visualization shows the relationship between contract types and churn rates. 
            It helps identify which contract arrangements are associated with higher customer retention."),
          highchartOutput(ns("monthly_trends"))
        )
      )
    ),
    
    # Seção de insights
    card(
      card_header("Key Insights"),
      div(
        class = "p-3",
        tags$ul(
          tags$li("Contract type is one of the strongest predictors of customer churn"),
          tags$li("Month-to-month contracts show significantly higher churn rates"),
          tags$li("Customers with higher monthly charges are more likely to churn"),
          tags$li("Technical support availability significantly impacts customer retention")
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data_processing$initialize_data()
    output$total_customers <- renderText({
      nrow(data$raw_data)
    })
    output$churn_rate <- renderText({
      paste0(
        round(mean(data$raw_data$Churn == "Yes") * 100, 1),
        "%"
      )
    })
    output$monthly_revenue <- renderText({
      paste0(
        "$",
        format(
          sum(data$raw_data$MonthlyCharges),
          big.mark = ",",
          scientific = FALSE
        )
      )
    })
    output$overall_churn <- renderHighchart({
      # Your existing chart code
      data$overall_churn |>
        hchart(
          hcaes(x = Customer, y = `% Customers`, group = Churn),
          type = "bar",
          stacking = "normal",
          dataLabels = list(enabled = TRUE)
        ) |>
        highcharter::hc_title(text = "Overall company's Churn") |>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(max = 100)
    })
    output$risk_factors <- renderHighchart({
      # Variables importance chart
      data$vars$importance |>
        highcharter::hchart(
          type = "bar",
          hcaes(x = variable, y = percentage * 100)
        ) |>
        highcharter::hc_title(text = "Variables Importance") |>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "Importance (%)"))
    })
    output$monthly_trends <- renderHighchart({
      # Monthly trends chart
      data$raw_data |>
        dplyr::group_by(Contract) |>
        dplyr::summarise(
          AvgCharges = mean(MonthlyCharges, na.rm = TRUE),
          ChurnRate = mean(Churn == "Yes") * 100
        ) |>
        highcharter::hchart(
          type = "column",
          hcaes(x = Contract, y = ChurnRate)
        ) |>
        highcharter::hc_title(text = "Churn Rate by Contract Type") |>
        highcharter::hc_yAxis(title = list(text = "Churn Rate (%)"))
    })
  })
}
