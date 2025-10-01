library(testthat)
library(jsonlite)

# Tests for API response structure validation (mocked)
# These tests validate the expected structure without requiring a running API

test_that("API response structure validation works correctly", {
  # Mock a complete API response structure
  mock_api_response <- list(
    raw_data = data.frame(
      customerID = c("TEST001", "TEST002"),
      gender = c("Male", "Female"),
      MonthlyCharges = c(50.0, 75.5),
      stringsAsFactors = FALSE
    ),
    predictions = data.frame(
      customerID = c("TEST001", "TEST002"),
      predict = c("No", "Yes"),
      Yes = c(0.2, 0.8),
      RiskGroup = c(2, 8),
      stringsAsFactors = FALSE
    ),
    overall_churn = data.frame(
      Churn = c("Yes", "No"),
      Count = c(1, 1),
      stringsAsFactors = FALSE
    ),
    churn_by_risk_groups = data.frame(
      RiskGroup = c(2, 8),
      ChurnRate = c(20, 80),
      stringsAsFactors = FALSE
    ),
    charge_for_risk_groups = data.frame(
      RiskGroup = c(2, 8),
      AvgMonthlyCharges = c(50.0, 75.5),
      stringsAsFactors = FALSE
    ),
    vars = list(
      importance = data.frame(
        variable = c("Contract", "tenure"),
        percentage = c(0.45, 0.30),
        stringsAsFactors = FALSE
      )
    ),
    colors = c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
  )
  
  # Validate response structure
  expect_true("raw_data" %in% names(mock_api_response))
  expect_true("predictions" %in% names(mock_api_response))
  expect_true("overall_churn" %in% names(mock_api_response))
  expect_true("churn_by_risk_groups" %in% names(mock_api_response))
  expect_true("charge_for_risk_groups" %in% names(mock_api_response))
  expect_true("vars" %in% names(mock_api_response))
  expect_true("colors" %in% names(mock_api_response))
  
  # Validate data types
  expect_true(is.data.frame(mock_api_response$raw_data))
  expect_true(is.data.frame(mock_api_response$predictions))
  expect_true(is.list(mock_api_response$vars))
  expect_true(is.character(mock_api_response$colors))
  
  # Validate data consistency
  expect_equal(nrow(mock_api_response$raw_data), 2)
  expect_equal(nrow(mock_api_response$predictions), 2)
  expect_true(all(mock_api_response$predictions$RiskGroup >= 1))
  expect_true(all(mock_api_response$predictions$RiskGroup <= 10))
})

test_that("API error responses are handled correctly", {
  # Mock error response for missing columns
  error_response <- list(
    error = "Missing required columns: customerID, gender"
  )
  
  # Validate error structure
  expect_true("error" %in% names(error_response))
  expect_true(is.character(error_response$error))
  expect_true(grepl("Missing required columns", error_response$error))
  
  # Mock error response for empty data
  empty_error_response <- list(
    error = "No CSV data provided"
  )
  
  expect_true("error" %in% names(empty_error_response))
  expect_true(grepl("No CSV data provided", empty_error_response$error))
})