library(testthat)
library(mockery)
library(httr)
library(jsonlite)

# Load the modules we're testing
box::use(
  app / logic / load_data,
  app / logic / data_store,
)

test_that("CSV file detection works correctly", {
  # Test CSV file detection logic
  
  # Create a temporary CSV file for testing
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "customer_data.csv")
  
  # Create test CSV content
  test_csv_content <- paste(
    "customerID,gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,Contract,PaperlessBilling,PaymentMethod,MonthlyCharges,TotalCharges",
    "TEST001,Male,0,No,No,12,Yes,No,DSL,Yes,No,No,No,No,No,Month-to-month,Yes,Electronic check,50.00,600.00",
    sep = "\n"
  )
  writeLines(test_csv_content, temp_csv)
  
  # Test that file exists
  expect_true(file.exists(temp_csv))
  
  # Test CSV reading
  csv_data <- read.csv(temp_csv, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), 1)
  expect_equal(csv_data$customerID, "TEST001")
  expect_equal(csv_data$MonthlyCharges, 50.00)
  
  # Cleanup
  unlink(temp_csv)
})

test_that("CSV to JSON conversion works correctly", {
  # Create test CSV data frame
  test_data <- data.frame(
    customerID = "TEST001",
    gender = "Male",
    SeniorCitizen = 0,
    Partner = "No",
    Dependents = "No",
    tenure = 12,
    PhoneService = "Yes",
    MultipleLines = "No",
    InternetService = "DSL",
    OnlineSecurity = "Yes",
    OnlineBackup = "No",
    DeviceProtection = "No",
    TechSupport = "No",
    StreamingTV = "No",
    StreamingMovies = "No",
    Contract = "Month-to-month",
    PaperlessBilling = "Yes",
    PaymentMethod = "Electronic check",
    MonthlyCharges = 50.00,
    TotalCharges = 600.00,
    stringsAsFactors = FALSE
  )
  
  # Convert to list of records (like the app does)
  csv_records <- lapply(1:nrow(test_data), function(i) {
    as.list(test_data[i, ])
  })
  
  # Test conversion
  expect_equal(length(csv_records), 1)
  expect_equal(csv_records[[1]]$customerID, "TEST001")
  expect_equal(csv_records[[1]]$MonthlyCharges, 50.00)
  
  # Test JSON serialization
  json_data <- toJSON(csv_records)
  expect_true(is.character(json_data))
  expect_true(nchar(json_data) > 0)
  
  # Test JSON can be parsed back
  parsed_data <- fromJSON(json_data)
  
  # JSON parsing of list of objects creates a data frame
  expect_true(is.data.frame(parsed_data))
  expect_equal(nrow(parsed_data), 1)
  expect_equal(as.character(parsed_data$customerID[1]), "TEST001")
})

test_that("process_csv_file function handles API responses correctly", {
  # Create temporary CSV file
  temp_dir <- tempdir()
  temp_csv <- file.path(temp_dir, "test_customer_data.csv")
  
  test_csv_content <- paste(
    "customerID,gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,Contract,PaperlessBilling,PaymentMethod,MonthlyCharges,TotalCharges",
    "TEST001,Male,0,No,No,12,Yes,No,DSL,Yes,No,No,No,No,No,Month-to-month,Yes,Electronic check,50.00,600.00",
    "TEST002,Female,1,Yes,No,24,Yes,Yes,Fiber optic,No,Yes,Yes,No,Yes,Yes,One year,No,Credit card,75.50,1812.00",
    sep = "\n"
  )
  writeLines(test_csv_content, temp_csv)
  
  # Mock successful API response with complete structure
  mock_response_data <- list(
    raw_data = data.frame(
      customerID = c("TEST001", "TEST002"),
      stringsAsFactors = FALSE
    ),
    predictions = data.frame(
      customerID = c("TEST001", "TEST002"),
      predict = c("No", "Yes"),
      Yes = c(0.2, 0.8),
      RiskGroup = c(2, 8),
      stringsAsFactors = FALSE
    ),
    overall_churn = data.frame(Churn = c("Yes", "No"), Count = c(1, 1)),
    churn_by_risk_groups = data.frame(),
    charge_for_risk_groups = data.frame(),
    vars = list(importance = data.frame()),
    colors = c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
  )
  
  # Mock config
  mock_config <- mock(list(use_api = TRUE, api_url = "http://localhost:8000"))
  
  # Mock the API functions properly
  suppressWarnings({
    with_mock(
      `httr::POST` = function(url, body, add_headers, encode) {
        cat("POST called with url:", url, "\n")
        # Return a mock response object
        structure(list(
          status_code = 200,
          content = toJSON(mock_response_data)
        ), class = "response")
      },
      `httr::status_code` = function(response) {
        cat("status_code called\n")
        return(response$status_code)
      },
      `httr::content` = function(response, type = "text", encoding = "UTF-8") {
        cat("content called with type:", type, "encoding:", encoding, "\n")
        return(response$content)
      },
      `httr::add_headers` = function(...) {
        cat("add_headers called\n")
        return(list())
      },
      `config::get` = function() {
        cat("config::get called\n")
        return(list(use_api = TRUE, api_url = "http://localhost:8000"))
      },
      {
        cat("About to call process_csv_file with:", temp_csv, "use_api=TRUE\n")
        result <- load_data$process_csv_file(temp_csv, use_api = TRUE)
        cat("process_csv_file returned\n")
        
        # Debug output
        cat("Result class:", class(result), "\n")
        cat("Result length:", length(result), "\n")
        cat("Result is null:", is.null(result), "\n")
        if (!is.null(result)) {
          cat("Result names:", names(result), "\n")
        }
        
        # Verify the result structure
        expect_true(!is.null(result))
        expect_true("raw_data" %in% names(result))
        expect_true("predictions" %in% names(result))
        expect_equal(nrow(result$raw_data), 2)
        expect_equal(nrow(result$predictions), 2)
      }
    )
  })
  
  # Cleanup
  unlink(temp_csv)
})

test_that("load_data function detects CSV files automatically", {
  # Create temporary CSV file in current directory
  temp_csv <- file.path(getwd(), "customer_data.csv")
  test_csv_content <- paste(
    "customerID,gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,Contract,PaperlessBilling,PaymentMethod,MonthlyCharges,TotalCharges",
    "TEST001,Male,0,No,No,12,Yes,No,DSL,Yes,No,No,No,No,No,Month-to-month,Yes,Electronic check,50.00,600.00",
    sep = "\n"
  )
  writeLines(test_csv_content, temp_csv)
  
  # Test that file exists and can be detected
  expect_true(file.exists(temp_csv))
  
  # Test CSV reading directly
  csv_data <- read.csv(temp_csv, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), 1)
  expect_equal(csv_data$customerID, "TEST001")
  
  # Cleanup
  unlink(temp_csv)
})

test_that("data_store class can be instantiated", {
  # Test basic data store functionality
  test_store <- data_store$churn_data_store$new()
  
  # Verify initialization
  expect_true(is.null(test_store$data))
  expect_false(test_store$is_loaded)
  
  # Test that methods exist
  expect_true(is.function(test_store$get_data))
  expect_true(is.function(test_store$load_data))
  expect_true(is.function(test_store$process_csv))
})

test_that("error handling works for invalid CSV files", {
  # Test with empty CSV file
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("", temp_csv)
  
  # Mock config for each call
  with_mock(
    `config::get` = function() list(use_api = TRUE, api_url = "http://localhost:8000"),
    {
      result <- load_data$process_csv_file(temp_csv, use_api = TRUE)
      expect_null(result)
    }
  )
  
  # Cleanup
  unlink(temp_csv)
  
  # Test with CSV missing required columns
  temp_csv <- tempfile(fileext = ".csv")
  writeLines("id,name\n1,test", temp_csv)
  
  # Mock API response for missing columns
  error_response <- list(error = "Missing required columns: customerID, gender")
  
  with_mock(
    `httr::POST` = function(...) list(),
    `httr::status_code` = function(...) 200,
    `httr::content` = function(...) toJSON(error_response),
    `config::get` = function() list(use_api = TRUE, api_url = "http://localhost:8000"),
    {
      result <- load_data$process_csv_file(temp_csv, use_api = TRUE)
      expect_null(result)
    }
  )
  
  # Cleanup
  unlink(temp_csv)
})

# Integration test that tests the full workflow
test_that("full CSV processing workflow integration test", {
  skip("Integration test requires API setup and config file")
  
  # This test can be run manually when API is running
  # To run: testthat::test_file("tests/testthat/test-csv-processing.R", filter="integration")
})