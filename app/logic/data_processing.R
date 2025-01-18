box::use(
  dplyr[`%>%`, mutate, group_by, summarise, across, where, n, filter, ntile,
        ungroup, tally, bind_cols, select, arrange, desc, everything],
  h2o[h2o.init, as.h2o, h2o.splitFrame, h2o.predict, h2o.gbm, h2o.varimp],
  readr[read_csv],
)

#' @export
initialize_data <- function() {
  # Initialize h2o
  h2o.init()
  # Read and process data
  data <- read_csv(
    "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv"
  ) %>%
    mutate(across(where(is.character), as.factor))
  # Create h2o frame and split data
  h2o_data <- as.h2o(data)
  splits <- h2o.splitFrame(h2o_data, ratios = 0.7)
  names(splits) <- c("train", "test")
  # Train model
  model <- h2o.gbm(
    x = setdiff(names(data), c("Churn", "customerID")),
    y = "Churn",
    training_frame = splits$train
  )
  # Get predictions
  predictions <- h2o.predict(model, splits$test)
  # Calculate overall churn
  overall_churn <- data %>%
    group_by(Churn) %>%
    summarise(
      Customer = n(),
      `% Customers` = round(100 * n() / nrow(data), 2)
    ) %>%
    mutate(Customer = "Churn Yes / No")

  # Get variable importance
  vars_importance <- h2o.varimp(model) %>%
    as.data.frame() %>%
    select(
      variable = variable,
      percentage = relative_importance
    )
  # Create predictions dataframe
  predictions_df <- as.data.frame(splits$test) %>%
    bind_cols(
      as.data.frame(predictions) %>%
        select(Predict = predict, PredictProbability = Yes) %>%
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) %>%
    mutate(
      RiskGroup = as.numeric(11 - ntile(PredictProbability, 10))
    ) %>%
    select(
      customerID, Churn, Predict, PredictProbability, RiskGroup, everything()
    ) %>%
    arrange(desc(PredictProbability))
  # Calculate churn by risk groups
  churn_by_risk_groups <- predictions_df %>%
    group_by(RiskGroup, Churn) %>%
    tally() %>%
    mutate(prop = 100 * n / sum(n)) %>%
    ungroup() %>%
    group_by(Churn) %>%
    mutate(
      prop_bad_good = 100 * n / sum(n),
      cum_prop = cumsum(prop_bad_good),
      n_cum_sum = cumsum(n)
    ) %>%
    ungroup() %>%
    group_by(RiskGroup) %>%
    mutate(precisao = 100 * n_cum_sum / sum(n_cum_sum)) %>%
    ungroup() %>%
    mutate(
      across(
        .cols = c("prop", "prop_bad_good", "cum_prop", "precisao"),
        .fns = round,
        2
      )
    )
  # Return list of data objects
  list(
    raw_data = data,
    predictions = predictions_df,
    overall_churn = overall_churn,
    churn_by_risk_groups = churn_by_risk_groups,
    charge_for_risk_groups = predictions_df %>%
      group_by(Churn, RiskGroup) %>%
      summarise(
        SumMonthlyCharges = sum(MonthlyCharges, na.rm = TRUE),
        .groups = "drop"
      ),
    vars = list(
      importance = vars_importance
    )
  )
}

#' @export
train_model <- function(data) {
  # Train GBM model
  model <- h2o::h2o.gbm(
    x = setdiff(names(data$raw_data), c("Churn", "customerID")),
    y = "Churn",
    training_frame = data$splits$train
  )
  model
}
