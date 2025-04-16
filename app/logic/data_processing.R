box::use(
  dplyr[`%>%`, across, arrange, bind_cols, desc, everything, group_by, mutate, n, ntile, 
        rename, select, summarise, tally, ungroup, where],
  h2o[as.h2o, h2o.gbm, h2o.init, h2o.performance, h2o.predict, h2o.splitFrame, h2o.varimp],
  readr[read_csv],
  tibble[as_tibble],
)

#' @export
initialize_data <- function() {
  # Check if cached data exists
  cache_path <- "data/model_output.rds"
  
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  # Create main container
  ml <- list()
  
  # Define colors for charts
  colors <- c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
  
  # Initialize h2o
  h2o.init()
  
  # Read and process data
  ml$data <- list()
  ml$data$raw <- read_csv(
    "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv"
  ) %>%
    mutate(across(where(is.character), as.factor))
  
  # Defining variables
  ml$vars <- list()
  ml$vars$y <- "Churn"
  ml$vars$discard <- "customerID"
  ml$vars$x <- setdiff(names(ml$data$raw), c(ml$vars$y, ml$vars$discard))
  
  # Setup h2o
  ml$data$h2o <- as.h2o(ml$data$raw)
  ml$data$splits <- h2o.splitFrame(ml$data$h2o, ratios = 0.7)
  names(ml$data$splits) <- c("train", "test")
  
  # Running the model
  ml$model <- h2o.gbm(x = ml$vars$x, y = ml$vars$y, training_frame = ml$data$splits$train)
  ml$predictions <- h2o.predict(ml$model, ml$data$splits$test)
  h2o.performance(ml$model, ml$data$splits$test)
  
  # Create predictions dataframe
  ml$data$predictions <- ml$data$splits$test %>%
    as_tibble() %>%
    bind_cols(
      as_tibble(ml$predictions) %>% 
        select(Predict = predict, PredictProbability = Yes) %>%
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) %>%
    # 11 is not a magic number, it is inverting the order of the deciles
    mutate(RiskGroup = as.factor(11 - ntile(PredictProbability, 10))) %>%
    select(customerID, Churn, Predict, PredictProbability, RiskGroup, everything()) %>%
    arrange(desc(PredictProbability))
  
  # Calculate overall churn
  ml$data$overall_churn <- ml$data$raw %>%
    group_by(Churn) %>%
    tally() %>%
    mutate(
      `% Customers` = round(100 * n / sum(n), 2),
      Customer = "Churn Yes / No"
    ) %>%
    rename(`Count Customers` = n)

  # Get variable importance - removed multiplication by 100
  ml$vars$importance <- h2o.varimp(ml$model) %>%
    as_tibble()

  # Calculate churn by risk groups
  ml$data$churn_by_risk_groups <- ml$data$predictions %>%
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
    
  # Calculate financial values for each risk group
  ml$data$charge_for_risk_groups <- ml$data$predictions %>%
    group_by(Churn, RiskGroup) %>%
    summarise(
      SumMonthlyCharges = sum(MonthlyCharges, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create output list with the same structure expected by the app
  output_list <- list(
    raw_data = ml$data$raw,
    predictions = ml$data$predictions,
    overall_churn = ml$data$overall_churn,
    churn_by_risk_groups = ml$data$churn_by_risk_groups,
    charge_for_risk_groups = ml$data$charge_for_risk_groups,
    vars = list(
      importance = ml$vars$importance
    ),
    colors = colors  # Add colors to the output list
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
