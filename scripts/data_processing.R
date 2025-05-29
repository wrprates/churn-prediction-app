# Package imports using library()
library("dplyr")
library("h2o")
library("readr")
library("tibble")

# Main function to initialize data and create the model
initialize_data <- function(save_model = TRUE) {
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
  ) |>
    mutate(across(where(is.character), as.factor))

  # Defining variables
  ml$vars <- list()
  ml$vars$y <- "Churn"
  ml$vars$discard <- "customerID"
  ml$vars$x <- setdiff(names(ml$data$raw), c(ml$vars$y, ml$vars$discard))

  # Setup h2o
  ml$data$h2o <- as.h2o(ml$data$raw)

  # Set a fixed random seed for reproducibility
  set.seed(123)

  # Use the seed parameter directly in splitFrame for consistent results
  ml$data$splits <- h2o.splitFrame(ml$data$h2o, ratios = 0.7, seed = 123)
  names(ml$data$splits) <- c("train", "test")

  # Running the model
  ml$model <- train_model(ml$vars$x, ml$vars$y, ml$data$splits$train)

  ml$predictions <- h2o.predict(ml$model, ml$data$splits$test)
  h2o.performance(ml$model, ml$data$splits$test)

  # Create predictions dataframe
  ml$data$predictions <- ml$data$splits$test |>
    as_tibble() |>
    bind_cols(
      as_tibble(ml$predictions) |>
        select(Predict = predict, PredictProbability = Yes) |>
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) |>
    # 11 is not a magic number, it is inverting the order of the deciles
    mutate(RiskGroup = as.factor(11 - ntile(PredictProbability, 10))) |>
    select(
      customerID,
      Churn,
      Predict,
      PredictProbability,
      RiskGroup,
      everything()
    ) |>
    arrange(desc(PredictProbability))

  # Calculate overall churn
  ml$data$overall_churn <- ml$data$raw |>
    group_by(Churn) |>
    tally() |>
    mutate(
      `% Customers` = round(100 * n / sum(n), 2),
      Customer = "Churn Yes / No"
    ) |>
    rename(`Count Customers` = n)

  # Get variable importance - removed multiplication by 100
  ml$vars$importance <- h2o.varimp(ml$model) |>
    as_tibble()

  # Calculate churn by risk groups
  ml$data$churn_by_risk_groups <- ml$data$predictions |>
    group_by(RiskGroup, Churn) |>
    tally() |>
    mutate(prop = 100 * n / sum(n)) |>
    ungroup() |>
    group_by(Churn) |>
    mutate(
      prop_bad_good = 100 * n / sum(n),
      cum_prop = cumsum(prop_bad_good),
      n_cum_sum = cumsum(n)
    ) |>
    ungroup() |>
    group_by(RiskGroup) |>
    mutate(precisao = 100 * n_cum_sum / sum(n_cum_sum)) |>
    ungroup() |>
    mutate(
      across(
        .cols = c("prop", "prop_bad_good", "cum_prop", "precisao"),
        \(x) round(x, 2)
      )
    )

  # Calculate financial values for each risk group
  ml$data$charge_for_risk_groups <- ml$data$predictions |>
    group_by(Churn, RiskGroup) |>
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
    colors = colors # Add colors to the output list
  )

  # Create data directory if it doesn't exist
  if (!dir.exists("api/data")) {
    dir.create("api/data", recursive = TRUE)
  }

  # Save output to RDS
  cache_path <- "api/data/model_output.rds"
  saveRDS(output_list, cache_path)

  # Save the model separately if requested
  if (save_model) {
    # First, remove any existing model directory to avoid conflicts
    model_dir <- "data/churn_model"
    if (dir.exists(model_dir)) {
      message("Removing existing model directory: ", model_dir)
      unlink(model_dir, recursive = TRUE)
    }

    # Save the model - this creates a directory with the proper structure
    model_path <- h2o.saveModel(ml$model, path = model_dir, force = TRUE, filename = "churn_model.h2o")
    message("H2O model saved to: ", model_path)
  }

  # Return the output
  output_list
}

# Function to train the model
train_model <- function(x_vars, y_var, training_frame) {
  # Train GBM model
  model <- h2o::h2o.gbm(
    x = x_vars,
    y = y_var,
    training_frame = training_frame
  )
  model
}

# Function to make predictions with a saved model
predict_with_model <- function(model, new_data) {
  # Convert data to h2o frame if it's not already
  if (!inherits(new_data, "H2OFrame")) {
    new_data <- as.h2o(new_data)
  }

  # Make predictions
  predictions <- h2o.predict(model, new_data)

  # Process predictions
  result <- as_tibble(new_data) |>
    bind_cols(
      as_tibble(predictions) |>
        select(Predict = predict, PredictProbability = Yes) |>
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) |>
    # 11 is not a magic number, it is inverting the order of the deciles
    mutate(RiskGroup = as.factor(11 - ntile(PredictProbability, 10))) |>
    arrange(desc(PredictProbability))

  return(result)
}

# Run the data processing when script is sourced or run directly
message("Starting data processing...")
result <- initialize_data(save_model = TRUE)
message("Processing complete and data saved to 'api/data/model_output.rds'")
message("H2O model saved to 'api/data/churn_model'")
