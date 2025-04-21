box::use(
  bslib[card, card_header, layout_column_wrap, page_fluid],
  dplyr[`%>%`, filter, distinct, pull],
  highcharter[hc_add_series, hc_xAxis, hc_yAxis, hcaes,
              hchart, highchartOutput, renderHighchart],
  htmltools[HTML],
  reactable[reactableOutput, renderReactable],
  shiny[moduleServer, NS, div, tags, reactive, observe, req],
  shinyWidgets[virtualSelectInput, updateVirtualSelect],
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
        card_header("Filter By"),
        div(
          class = "p-3",
          layout_column_wrap(
            width = 1 / 3,
            virtualSelectInput(
              inputId = ns("filter_contract"),
              label = "Contract Type",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE
            ),
            virtualSelectInput(
              inputId = ns("filter_payment"),
              label = "Payment Method",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE
            ),
            virtualSelectInput(
              inputId = ns("filter_tech"),
              label = "Tech Support",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE
            )
          )
        )
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

    # Update filter choices based on available data
    observe({
      # Use req to ensure data is available before proceeding
      req(data, data$predictions, nrow(data$predictions) > 0)

      # Get unique values for each filter
      contract_choices <- data$predictions %>%
        filter(RiskGroup %in% c("1", "2", "3")) %>%
        distinct(Contract) %>%
        pull(Contract)

      payment_choices <- data$predictions %>%
        filter(RiskGroup %in% c("1", "2", "3")) %>%
        distinct(PaymentMethod) %>%
        pull(PaymentMethod)

      tech_choices <- data$predictions %>%
        filter(RiskGroup %in% c("1", "2", "3")) %>%
        distinct(TechSupport) %>%
        pull(TechSupport)

      # Update virtual select inputs without conditional checks
      # The NS function is used internally by updateVirtualSelect
      # to handle namespacing in modules
      updateVirtualSelect(
        session = session,
        inputId = "filter_contract",
        choices = contract_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_payment",
        choices = payment_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_tech",
        choices = tech_choices,
        selected = NULL
      )
    })

    # Filtered data reactive
    filtered_data <- reactive({
      # Use req to ensure data is available
      req(data, data$predictions)

      result <- data$predictions %>%
        filter(RiskGroup %in% c("1", "2", "3"))

      # Apply filters only if selections are made, using %in% for multiple selections
      if (!is.null(input$filter_contract) && length(input$filter_contract) > 0) {
        result <- result %>% filter(Contract %in% input$filter_contract)
      }

      if (!is.null(input$filter_payment) && length(input$filter_payment) > 0) {
        result <- result %>% filter(PaymentMethod %in% input$filter_payment)
      }

      if (!is.null(input$filter_tech) && length(input$filter_tech) > 0) {
        result <- result %>% filter(TechSupport %in% input$filter_tech)
      }

      return(result)
    })

    output$risk_groups_churn <- renderHighchart({
      # Use req to ensure data is available
      req(data, data$churn_by_risk_groups, nrow(data$churn_by_risk_groups) > 0)

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
      # Get filtered data and use req to ensure it exists
      filtered <- filtered_data()
      req(filtered, nrow(filtered) > 0)

      filtered |>
        reactable::reactable(
          columns = list(
            Contract = reactable::colDef(minWidth = 150),
            PaymentMethod = reactable::colDef(minWidth = 150),
            TechSupport = reactable::colDef(minWidth = 150),
            PredictProbability = reactable::colDef(
              minWidth = 150,
              cell = function(value) {
                # Divide by 100 to convert to proper percentage value
                adjusted_value <- value / 100
                # Format to 2 decimal places
                paste0(format(round(adjusted_value * 100, 2), nsmall = 2), "%")
              }
            )
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
