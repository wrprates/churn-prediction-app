library(httr)
library(jsonlite)
library(dplyr)

# API base URL
base_url <- "http://localhost:8000"

# Headers
headers <- c(
  "Content-Type" = "application/json"
)

# Function to print response
print_response <- function(response, title) {
  cat("\n", title, ":\n")
  print(fromJSON(rawToChar(response$content)))
}

# Function to convert list to data frame
list_to_df <- function(data) {
  if (is.list(data) && !is.data.frame(data)) {
    if (!any(sapply(data, is.list))) {
      return(as.data.frame(t(unlist(data)), stringsAsFactors = FALSE))
    } else {
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

# Create a single customer data object with explicit numeric types
customer_data <- list(
  customerID = "1234",
  gender = "Male",
  SeniorCitizen = as.numeric(0),
  Partner = "Yes",
  Dependents = "No",
  tenure = as.numeric(24),
  PhoneService = "Yes",
  MultipleLines = "No",
  InternetService = "DSL",
  OnlineSecurity = "Yes",
  OnlineBackup = "No",
  DeviceProtection = "Yes",
  TechSupport = "No",
  StreamingTV = "Yes",
  StreamingMovies = "No",
  Contract = "Month-to-month",
  PaperlessBilling = "Yes",
  PaymentMethod = "Electronic check",
  MonthlyCharges = as.numeric(65.5),
  TotalCharges = as.numeric(1572.0)
)

# Convert to JSON string with numeric precision
customer_json <- toJSON(customer_data, auto_unbox = TRUE, digits = 10)

# Test 1: Validate data
cat("\n=== Test 1: Validate Data ===\n")
response <- POST(
  paste0(base_url, "/model/validate-data"),
  add_headers(.headers = headers),
  body = customer_json
)
print_response(response, "Validation response")

# Test 2: Preprocess data
cat("\n=== Test 2: Preprocess Data ===\n")
response <- POST(
  paste0(base_url, "/model/preprocess"),
  add_headers(.headers = headers),
  body = customer_json
)
print_response(response, "Preprocess response")

# Test 3: Make predictions
cat("\n=== Test 3: Make Predictions ===\n")
response <- POST(
  paste0(base_url, "/model/predict"),
  add_headers(.headers = headers),
  body = customer_json
)
print_response(response, "Prediction response")

# Test 4: Process and predict
cat("\n=== Test 4: Process and Predict ===\n")
response <- POST(
  paste0(base_url, "/model/process-and-predict"),
  add_headers(.headers = headers),
  body = customer_json
)
print_response(response, "Process and predict response")

# Test 5: Batch predict
cat("\n=== Test 5: Batch Predict ===\n")
# Create batch data as an array of customer objects with explicit numeric types
batch_data <- list(
  list(
    customerID = "1234",
    gender = "Male",
    SeniorCitizen = as.numeric(0),
    Partner = "Yes",
    Dependents = "No",
    tenure = as.numeric(24),
    PhoneService = "Yes",
    MultipleLines = "No",
    InternetService = "DSL",
    OnlineSecurity = "Yes",
    OnlineBackup = "No",
    DeviceProtection = "Yes",
    TechSupport = "No",
    StreamingTV = "Yes",
    StreamingMovies = "No",
    Contract = "Month-to-month",
    PaperlessBilling = "Yes",
    PaymentMethod = "Electronic check",
    MonthlyCharges = as.numeric(65.5),
    TotalCharges = as.numeric(1572.0)
  ),
  list(
    customerID = "5678",
    gender = "Female",
    SeniorCitizen = as.numeric(1),
    Partner = "No",
    Dependents = "Yes",
    tenure = as.numeric(12),
    PhoneService = "Yes",
    MultipleLines = "Yes",
    InternetService = "Fiber optic",
    OnlineSecurity = "No",
    OnlineBackup = "Yes",
    DeviceProtection = "No",
    TechSupport = "Yes",
    StreamingTV = "No",
    StreamingMovies = "Yes",
    Contract = "Two year",
    PaperlessBilling = "No",
    PaymentMethod = "Credit card",
    MonthlyCharges = as.numeric(89.9),
    TotalCharges = as.numeric(1078.8)
  )
)

# Convert batch data to JSON with numeric precision
batch_json <- toJSON(batch_data, auto_unbox = TRUE, digits = 10)

response <- POST(
  paste0(base_url, "/model/batch-predict"),
  add_headers(.headers = headers),
  body = batch_json
)
print_response(response, "Batch predict response")

# Test 6: Calculate risk metrics
cat("\n=== Test 6: Calculate Risk Metrics ===\n")
# First get some predictions
pred_response <- POST(
  paste0(base_url, "/model/predict"),
  add_headers(.headers = headers),
  body = customer_json
)
predictions <- fromJSON(rawToChar(pred_response$content))

# Convert predictions to data frame if it's a list
if (is.list(predictions) && !is.data.frame(predictions)) {
  predictions <- as.data.frame(predictions, stringsAsFactors = FALSE)
}

# Add Churn column if not present
if (!"Churn" %in% names(predictions)) {
  predictions$Churn <- predictions$Predict
}

# Convert predictions to JSON
predictions_json <- toJSON(predictions, auto_unbox = TRUE, digits = 10)

# Then calculate risk metrics
response <- POST(
  paste0(base_url, "/model/calculate-risk"),
  add_headers(.headers = headers),
  body = predictions_json
)
print_response(response, "Risk metrics response")

# Test 7: Check pipeline status
cat("\n=== Test 7: Pipeline Status ===\n")
response <- GET(paste0(base_url, "/model/pipeline-status"))
print_response(response, "Pipeline status")

if (is.list(customer_data) && !is.data.frame(customer_data)) {
  customer_data <- as.data.frame(customer_data, stringsAsFactors = FALSE)
}

# Add numeric field validation
validation_errors <- list()
for (field in names(customer_data)) {
  if (!is.numeric(customer_data[[field]])) {
    customer_data[[field]] <- as.numeric(customer_data[[field]])
    if (is.na(customer_data[[field]])) {
      validation_errors[[field]] <- paste(field, "must be numeric")
    }
  }
  if (!is.character(customer_data[[field]]) && !is.factor(customer_data[[field]])) {
    customer_data[[field]] <- as.character(customer_data[[field]])
  }
}
