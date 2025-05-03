box::use(
  bslib[card, card_header, layout_column_wrap, page_fluid],
  dplyr[filter, distinct, pull],
  highcharter[hc_add_series, hc_xAxis, hc_yAxis, hcaes,
              hchart, highchartOutput, renderHighchart],
  htmltools[HTML],
  reactable[reactable, reactableOutput, renderReactable, colDef, colFormat],
  rlang[sym],
  shiny[moduleServer, NS, div, tags, reactive, observe, req],
  shinyWidgets[virtualSelectInput, updateVirtualSelect],
)

box::use(
  app/logic/load_data,
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
        div(
          class = "p-3",
          div(
            class = "mb-3",
            tags$h5("Filters", class = "text-muted")
          ),
          layout_column_wrap(
            width = 1 / 3,
            virtualSelectInput(
              inputId = ns("filter_churn"),
              label = "Churned",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            ),
            virtualSelectInput(
              inputId = ns("filter_contract"),
              label = "Contract Type",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            ),
            virtualSelectInput(
              inputId = ns("filter_tenure"),
              label = "Tenure",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            )
          ),
          layout_column_wrap(
            width = 1 / 3,
            virtualSelectInput(
              inputId = ns("filter_internet"),
              label = "Internet Service",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            ),
            virtualSelectInput(
              inputId = ns("filter_online_security"),
              label = "Online Security",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            ),
            virtualSelectInput(
              inputId = ns("filter_payment"),
              label = "Payment Method",
              choices = NULL,
              search = TRUE,
              width = "100%",
              multiple = TRUE,
              clearButton = TRUE,
              zIndex = 9999
            )
          )
        ),
        tags$hr(class = "mt-2 mb-4"),
        reactableOutput(ns("risk_table"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- load_data$load_data()

    is_filter_active <- function(filter_value) {
      !is.null(filter_value) && length(filter_value) > 0
    }


    apply_filter <- function(data_frame, filter_value, column) {
      if (is_filter_active(filter_value)) {
        data_frame |> filter(!!sym(column) %in% filter_value)
      }
      data_frame
    }

    observe({
      req(data, data$predictions, nrow(data$predictions) > 0)

      churn_choices <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        distinct(Churn) |>
        pull(Churn)

      contract_choices <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        distinct(Contract) |>
        pull(Contract)

      tenure_min <- 0
      tenure_max <- 72
      tenure_breaks <- c(0, 12, 24, 36, 48, 60, 72)
      tenure_labels <- c("0-12", "13-24", "25-36", "37-48", "49-60", "61-72")

      tenure_values <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        pull(tenure) |>
        cut(breaks = c(0, 12, 24, 36, 48, 60, 72), labels = c("0-12", "13-24", "25-36", "37-48", "49-60", "61-72"))
      tenure_choices <- levels(tenure_values)

      payment_choices <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        distinct(PaymentMethod) |>
        pull(PaymentMethod)

      internet_choices <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        distinct(InternetService) |>
        pull(InternetService)

      online_security_choices <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3")) |>
        distinct(OnlineSecurity) |>
        pull(OnlineSecurity)

      updateVirtualSelect(
        session = session,
        inputId = "filter_churn",
        choices = churn_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_contract",
        choices = contract_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_tenure",
        choices = tenure_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_internet",
        choices = internet_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_online_security",
        choices = online_security_choices,
        selected = NULL
      )

      updateVirtualSelect(
        session = session,
        inputId = "filter_payment",
        choices = payment_choices,
        selected = NULL
      )
    })

    filtered_data <- reactive({
      req(data, data$predictions)

      result <- data$predictions |>
        filter(RiskGroup %in% c("1", "2", "3"))

      filters <- list(
        list(value = input$filter_churn, column = "Churn"),
        list(value = input$filter_contract, column = "Contract"),
        list(value = input$filter_payment, column = "PaymentMethod"),
        list(value = input$filter_internet, column = "InternetService"),
        list(value = input$filter_online_security, column = "OnlineSecurity")
      )

      if (is_filter_active(input$filter_tenure)) {
        selected_intervals <- input$filter_tenure
        filtered_result <- data.frame()

        for (range_str in selected_intervals) {
          range_limits <- as.numeric(unlist(strsplit(range_str, "-")))

          if (length(range_limits) == 2) {
            min_val <- range_limits[1]
            max_val <- range_limits[2]

            interval_result <- result |>
              filter(tenure >= min_val & tenure <= max_val)

            if (nrow(filtered_result) == 0) {
              filtered_result <- interval_result
            } else {
              filtered_result <- rbind(filtered_result, interval_result)
            }
          }
        }

        if (nrow(filtered_result) > 0) {
          result <- filtered_result |> distinct()
        }
      }

      for (filter_item in filters) {
        result <- apply_filter(result, filter_item$value, filter_item$column)
      }

      return(result)
    })

    output$risk_groups_churn <- renderHighchart({
      req(data, data$churn_by_risk_groups, nrow(data$churn_by_risk_groups) > 0)

      data$churn_by_risk_groups |>
        hchart(
          hcaes(x = RiskGroup, y = prop, group = Churn),
          type = "column"
        ) |>
        hc_add_series(
          name = "Cumulative % of canceled customers (recall)",
          data = (data$churn_by_risk_groups |>
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
      filtered <- filtered_data()
      req(filtered, nrow(filtered) > 0)

      filtered |>
        reactable(
          columns = list(
            customerID = colDef(name = "Customer ID", minWidth = 150),
            Churn = colDef(name = "Churned", minWidth = 100),
            Predict = colDef(name = "Prediction", minWidth = 120),
            PredictProbability = colDef(
              name = "Churn Probability",
              minWidth = 150,
              cell = function(value) {
                # Divide by 100 to convert to proper percentage value
                adjusted_value <- value / 100
                # Format to 2 decimal places
                paste0(format(round(adjusted_value * 100, 2), nsmall = 2), "%")
              }
            ),
            RiskGroup = colDef(name = "Risk Group", minWidth = 120),
            gender = colDef(name = "Gender", minWidth = 100),
            SeniorCitizen = colDef(name = "Senior Citizen", minWidth = 130),
            Partner = colDef(name = "Partner", minWidth = 100),
            Dependents = colDef(name = "Dependents", minWidth = 120),
            tenure = colDef(name = "Tenure (months)", minWidth = 140),
            PhoneService = colDef(name = "Phone Service", minWidth = 130),
            MultipleLines = colDef(name = "Multiple Lines", minWidth = 130),
            InternetService = colDef(name = "Internet Service", minWidth = 150),
            OnlineSecurity = colDef(name = "Online Security", minWidth = 150),
            OnlineBackup = colDef(name = "Online Backup", minWidth = 150),
            DeviceProtection = colDef(name = "Device Protection", minWidth = 160),
            TechSupport = colDef(name = "Tech Support", minWidth = 150),
            StreamingTV = colDef(name = "Streaming TV", minWidth = 130),
            StreamingMovies = colDef(name = "Streaming Movies", minWidth = 150),
            Contract = colDef(name = "Contract Type", minWidth = 150),
            PaperlessBilling = colDef(name = "Paperless Billing", minWidth = 150),
            PaymentMethod = colDef(name = "Payment Method", minWidth = 150),
            MonthlyCharges = colDef(
              name = "Monthly Charges",
              minWidth = 150,
              format = colFormat(prefix = "$", digits = 2)
            ),
            TotalCharges = colDef(
              name = "Total Charges",
              minWidth = 150,
              format = colFormat(prefix = "$", digits = 2)
            )
          ),
          highlight = TRUE,
          striped = FALSE,
          filterable = FALSE,
          searchable = FALSE,
          compact = TRUE,
          defaultPageSize = 10
        )
    })
  })
}
