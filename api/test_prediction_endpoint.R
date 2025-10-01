#!/usr/bin/env Rscript

# Test script for the new prediction endpoint in the Churn Prediction API
# This script demonstrates how to make a POST request to the prediction endpoint
# with sample customer data

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(h2o)

# Define the API URL - modify if needed
API_URL <- "http://localhost:8000"

# Function to test API connection
test_api_connection <- function() {
  message("Testing API connection...")

  response <- tryCatch(
    {
      GET(paste0(API_URL, "/"))
    },
    error = function(e) {
      message("Error connecting to API: ", e$message)
      message("Make sure the API is running at ", API_URL)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(FALSE)
  }

  if (http_status(response)$category == "Success") {
    content <- content(response, "parsed")
    message("API connection successful.")
    message("API version: ", content$version)
    message("Model status: ", content$modelStatus)
    return(TRUE)
  } else {
    message("API connection failed with status code: ", response$status_code)
    return(FALSE)
  }
}

# Function to test prediction endpoint with a single customer
test_single_prediction <- function() {
  message("\n--- Testing prediction with a single customer ---")

  # Create sample customer data
  customer <- list(
    customerID = "test-customer-001",
    gender = "Female",
    SeniorCitizen = 0,
    Partner = "Yes",
    Dependents = "No",
    tenure = 24,
    PhoneService = "Yes",
    MultipleLines = "Yes",
    InternetService = "Fiber optic",
    OnlineSecurity = "No",
    OnlineBackup = "Yes",
    DeviceProtection = "No",
    TechSupport = "No",
    StreamingTV = "Yes",
    StreamingMovies = "Yes",
    Contract = "Month-to-month",
    PaperlessBilling = "Yes",
    PaymentMethod = "Electronic check",
    MonthlyCharges = 90.45,
    TotalCharges = 2171.80
  )

  # Ensure the customer data has the expected columns
  expected_cols <- c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges")
  customer <- customer[expected_cols]

  # Convert to JSON
  json_data <- toJSON(customer, auto_unbox = TRUE)

  # Make prediction request
  response <- tryCatch(
    {
      POST(
        paste0(API_URL, "/predict"),
        body = json_data,
        content_type("application/json"),
        encode = "raw"
      )
    },
    error = function(e) {
      message("Error making prediction request: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(FALSE)
  }

  # Process response
  if (http_status(response)$category == "Success") {
    result <- content(response, "parsed")
    if ("error" %in% names(result)) {
      message("API returned error: ", result$error)
      return(FALSE)
    }

    message("Prediction successful:")
    message("  Predicted churn: ", result$Predict)
    message("  Churn probability: ", result$PredictProbability, "%")
    message("  Risk group: ", result$RiskGroup)
    return(TRUE)
  } else {
    message("Prediction failed with status code: ", response$status_code)
    if (http_status(response)$category == "Client error") {
      message("Response content: ", content(response, "text"))
    }
    return(FALSE)
  }
}

# Function to test prediction endpoint with multiple customers
test_multiple_predictions <- function() {
  message("\n--- Testing prediction with multiple customers ---")

  # Try to load some sample data from the raw data source
  sample_data <- tryCatch(
    {
      read_csv(
        "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv",
        n_max = 5
      ) |>
        select(-Churn) # Remove the Churn column since we're predicting it
    },
    error = function(e) {
      message("Error loading sample data: ", e$message)

      # Create sample customers manually if loading fails
      data.frame(
        customerID = c("sample-001", "sample-002", "sample-003"),
        gender = c("Female", "Male", "Female"),
        SeniorCitizen = c(0, 1, 0),
        Partner = c("Yes", "No", "Yes"),
        Dependents = c("No", "No", "Yes"),
        tenure = c(24, 12, 36),
        PhoneService = c("Yes", "Yes", "Yes"),
        MultipleLines = c("Yes", "No", "No"),
        InternetService = c("Fiber optic", "DSL", "No"),
        Contract = c("Month-to-month", "One year", "Two year"),
        MonthlyCharges = c(90.45, 65.3, 45.2),
        TotalCharges = c(2171.80, 783.6, 1627.2),
        stringsAsFactors = FALSE
      )
    }
  )

  # Select only the expected columns
  expected_cols <- c(
    "gender", "SeniorCitizen", "Partner", "Dependents", "tenure", "PhoneService", "MultipleLines",
    "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
    "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
    "MonthlyCharges", "TotalCharges"
  )
  sample_data <- sample_data[, expected_cols]

  # Convert to list for JSON conversion
  customers_list <- lapply(seq_len(nrow(sample_data)), function(i) {
    as.list(sample_data[i, ])
  })

  # Convert to JSON
  json_data <- toJSON(customers_list, auto_unbox = TRUE)

  # Make prediction request
  response <- tryCatch(
    {
      POST(
        paste0(API_URL, "/predict_batch"),
        body = json_data,
        content_type("application/json"),
        encode = "raw"
      )
    },
    error = function(e) {
      message("Error making prediction request: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(FALSE)
  }

  # Process response
  if (http_status(response)$category == "Success") {
    result <- content(response, "parsed")
    if ("error" %in% names(result)) {
      message("API returned error: ", result$error)
      return(FALSE)
    }

    message("Multiple predictions successful:")
    message("  Number of predictions: ", length(result))

    # Print summary of predictions
    for (i in 1:min(3, length(result))) {
      message("  Customer ", i, " (", result[[i]]$customerID, "):")
      message("    Predicted churn: ", result[[i]]$Predict)
      message("    Churn probability: ", result[[i]]$PredictProbability, "%")
      message("    Risk group: ", result[[i]]$RiskGroup)
    }

    if (length(result) > 3) {
      message("  ... and ", length(result) - 3, " more")
    }

    return(TRUE)
  } else {
    message("Multiple predictions failed with status code: ", response$status_code)
    message("Response content: ", content(response, "text"))
    return(FALSE)
  }
}

# Main function
main <- function() {
  message("Churn Prediction API Test Script")
  message("===============================")

  # Test connection to API
  if (!test_api_connection()) {
    message("Aborting tests due to API connection failure.")
    return(FALSE)
  }

  # Test prediction endpoint with a single customer
  single_test <- test_single_prediction()

  # Test prediction endpoint with multiple customers
  multiple_test <- test_multiple_predictions()

  # Report overall results
  message("\n--- Test Results ---")
  message("API connection test: ", if (TRUE) "PASSED" else "FAILED")
  message("Single prediction test: ", if (single_test) "PASSED" else "FAILED")
  message("Multiple predictions test: ", if (multiple_test) "PASSED" else "FAILED")

  message("\nAll tests ", if (single_test && multiple_test) "PASSED" else "FAILED")
  return(single_test && multiple_test)
}

# Run the tests
main()
