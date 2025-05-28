# Data processing functions for real-time API usage
library(dplyr)
library(h2o)
library(tibble)
library(jsonlite)

#' Convert list to data frame
#' @param data A list or data frame
#' @return A data frame
list_to_df <- function(data) {
  if (is.list(data) && !is.data.frame(data)) {
    # If it's a single customer (list of values), convert to a single-row data frame
    if (!any(sapply(data, is.list))) {
      return(as.data.frame(t(unlist(data)), stringsAsFactors = FALSE))
    } else {
      # If it's a list of customers, use do.call to bind them
      return(do.call(
        rbind,
        lapply(data, function(x) {
          as.data.frame(x, stringsAsFactors = FALSE)
        })
      ))
    }
  }
  return(data)
}

#' Validate customer data structure and content
#' @param customer_data A JSON object containing customer data
#' @return A list with validation results
validate_customer_data <- function(customer_data) {
  # Expected columns from the original dataset
  expected_columns <- c(
    "customerID",
    "gender",
    "SeniorCitizen",
    "Partner",
    "Dependents",
    "tenure",
    "PhoneService",
    "MultipleLines",
    "InternetService",
    "OnlineSecurity",
    "OnlineBackup",
    "DeviceProtection",
    "TechSupport",
    "StreamingTV",
    "StreamingMovies",
    "Contract",
    "PaperlessBilling",
    "PaymentMethod",
    "MonthlyCharges",
    "TotalCharges"
  )

  # Convert to data frame using the list_to_df function
  customer_data <- list_to_df(customer_data)

  # Check if all required columns are present
  missing_columns <- setdiff(expected_columns, names(customer_data))
  if (length(missing_columns) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_columns, collapse = ", "))
    ))
  }

  # Validate data types and ranges
  validation_errors <- list()

  # Check numeric fields
  numeric_fields <- c("tenure", "MonthlyCharges", "TotalCharges", "SeniorCitizen")
  for (field in numeric_fields) {
    if (!is.numeric(customer_data[[field]])) {
      customer_data[[field]] <- as.numeric(customer_data[[field]])
      if (is.na(customer_data[[field]])) {
        validation_errors[[field]] <- paste(field, "must be numeric")
      }
    }
  }

  # Check categorical fields
  categorical_fields <- setdiff(expected_columns, numeric_fields)
  for (field in categorical_fields) {
    if (!is.character(customer_data[[field]]) && !is.factor(customer_data[[field]])) {
      customer_data[[field]] <- as.character(customer_data[[field]])
    }
  }

  # Check for missing values
  na_counts <- sapply(customer_data, function(x) sum(is.na(x)))
  if (any(na_counts > 0)) {
    validation_errors[["missing_values"]] <- paste(
      "Fields with missing values:",
      paste(names(na_counts)[na_counts > 0], collapse = ", ")
    )
  }

  # Return validation results
  if (length(validation_errors) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Validation errors:", paste(unlist(validation_errors), collapse = "; "))
    ))
  }

  return(list(valid = TRUE, message = "Data validation successful"))
}

#' Process customer data for prediction
#' @param customer_data A validated JSON object containing customer data
#' @return A processed data frame ready for prediction
process_customer_data <- function(customer_data) {
  # Convert to data frame using the list_to_df function
  customer_data <- list_to_df(customer_data)

  # Ensure SeniorCitizen is numeric
  if (!is.numeric(customer_data$SeniorCitizen)) {
    customer_data$SeniorCitizen <- as.numeric(customer_data$SeniorCitizen)
  }

  # Convert all character columns to factors, except SeniorCitizen
  for (col in names(customer_data)) {
    if (is.character(customer_data[[col]]) && col != "SeniorCitizen") {
      customer_data[[col]] <- as.factor(customer_data[[col]])
    }
  }

  # Convert numeric fields
  numeric_fields <- c("tenure", "MonthlyCharges", "TotalCharges")
  for (field in numeric_fields) {
    if (!is.numeric(customer_data[[field]])) {
      customer_data[[field]] <- as.numeric(customer_data[[field]])
    }
  }

  # Handle missing values in TotalCharges
  if (any(is.na(customer_data$TotalCharges))) {
    customer_data$TotalCharges[is.na(customer_data$TotalCharges)] <-
      customer_data$MonthlyCharges[is.na(customer_data$TotalCharges)]
  }

  return(customer_data)
}

#' Make predictions on processed customer data
#' @param model The H2O model to use for predictions
#' @param processed_data The processed customer data
#' @return A data frame with predictions
make_predictions <- function(model, processed_data) {
  # Ensure SeniorCitizen is numeric before converting to H2O frame
  if (!is.numeric(processed_data$SeniorCitizen)) {
    processed_data$SeniorCitizen <- as.numeric(processed_data$SeniorCitizen)
  }

  # Convert to H2O frame
  h2o_frame <- as.h2o(processed_data)

  # Make predictions
  predictions <- h2o.predict(model, h2o_frame)

  # Process the predictions
  result <- processed_data |>
    tibble::as_tibble() |>
    bind_cols(
      as_tibble(predictions) |>
        select(Predict = predict, PredictProbability = Yes) |>
        mutate(PredictProbability = round(100 * PredictProbability, 2))
    ) |>
    mutate(RiskGroup = as.factor(11 - ntile(PredictProbability, 10))) |>
    arrange(desc(PredictProbability))

  return(result)
}

#' Calculate risk metrics for predictions
#' @param predictions A JSON object containing predictions
#' @return A list with risk metrics
calculate_risk_metrics <- function(predictions) {
  # Convert to data frame using the list_to_df function
  predictions <- list_to_df(predictions)

  # Ensure numeric fields are numeric
  numeric_fields <- c("PredictProbability", "MonthlyCharges", "TotalCharges")
  for (field in numeric_fields) {
    if (field %in% names(predictions) && !is.numeric(predictions[[field]])) {
      predictions[[field]] <- as.numeric(predictions[[field]])
    }
  }

  # Add Churn column if not present
  if (!"Churn" %in% names(predictions)) {
    predictions$Churn <- predictions$Predict
  }

  # Calculate churn by risk groups
  churn_by_risk <- predictions |>
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

  # Calculate financial impact
  financial_impact <- predictions |>
    group_by(Churn, RiskGroup) |>
    summarise(
      SumMonthlyCharges = sum(MonthlyCharges, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    churn_by_risk = churn_by_risk,
    financial_impact = financial_impact
  ))
}
