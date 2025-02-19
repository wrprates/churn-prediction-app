box::use(
  dplyr[`%>%`, across, arrange, group_by, mutate, select, summarise, ungroup],
  h2o[as.h2o, h2o.gbm, h2o.init, h2o.predict, h2o.splitFrame, h2o.varimp],
  readr[read_csv],
)

#' @export
initialize_data <- function() {
  # Check if cached data exists
  cache_path <- "data/model_output.rds"
  
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  
  # Initialize h2o
  h2o.init()
  # Read and process data
  data <- read_csv(
    "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv"
  ) %>%
    mutate(across(dplyr::where(is.character), as.factor))
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
      Customer = dplyr::n(),
      `% Customers` = round(100 * dplyr::n() / nrow(data), 2)
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
    dplyr::bind_cols(
      as.data.frame(predictions) %>%
        select(Predict = predict, PredictProbability = Yes) %>%
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) %>%
    mutate(
      RiskGroup = as.numeric(11 - dplyr::ntile(PredictProbability, 10))
    ) %>%
    select(
      customerID, Churn, Predict, PredictProbability, RiskGroup, dplyr::everything()
    ) %>%
    arrange(dplyr::desc(PredictProbability))
  # Calculate churn by risk groups
  churn_by_risk_groups <- predictions_df %>%
    group_by(RiskGroup, Churn) %>%
    dplyr::tally() %>%
    mutate(prop = 100 * dplyr::n() / sum(dplyr::n())) %>%
    ungroup() %>%
    group_by(Churn) %>%
    mutate(
      prop_bad_good = 100 * dplyr::n() / sum(dplyr::n()),
      cum_prop = cumsum(prop_bad_good),
      n_cum_sum = cumsum(dplyr::n())
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
    
  # Create output list
  output_list <- list(
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
  
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Save output to RDS
  saveRDS(output_list, cache_path)
  
  # Return the output
  output_list
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
