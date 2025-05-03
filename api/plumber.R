# Self-contained Plumber API for Churn Prediction
# Combines functionality from both plumber.R and api.R into a single file

library(plumber)
library(dplyr)

# Initialize data variable
loaded_data <- NULL

#* @apiTitle Churn Prediction API
#* @apiDescription API for accessing churn prediction model data
#* @apiVersion 1.0.0

# Plumber router function
#* @plumber
function(pr) {
  # Find the data file, checking multiple possible locations
  find_data_file <- function() {
    # Get the current directory
    script_dir <- getwd()
    message("Current working directory: ", script_dir)

    # Try different possible locations for the data file using only relative paths
    possible_paths <- c(
      file.path("data", "model_output.rds"),                      # relative from current dir
      file.path("..", "data", "model_output.rds"),                # up one level
      file.path(".", "data", "model_output.rds"),                 # explicit current dir
      "model_output.rds"                                          # directly in current dir
    )

    # Try each path
    for (path in possible_paths) {
      message("Checking for data file at: ", path)
      if (file.exists(path)) {
        message("Found data file at: ", path)
        return(path)
      }
    }

    # If not found, as a last resort, try to find it by searching upwards
    # This avoids using absolute paths but still finds the file
    current <- script_dir
    max_levels <- 3  # Limit how far up we'll search

    for (i in 1:max_levels) {
      test_path <- file.path(current, "data", "model_output.rds")
      message("Trying path: ", test_path)
      if (file.exists(test_path)) {
        message("Found data file at: ", test_path)
        # Return a relative path by calculating difference from current dir
        rel_path <- file.path(paste(rep("..", i), collapse = "/"), "data", "model_output.rds")
        message("Using relative path: ", rel_path)
        return(rel_path)
      }
      # Move up one directory
      current <- dirname(current)
    }

    stop("Could not find model_output.rds file. Please ensure it exists in the data directory.")
  }

  # Get the correct path to the model data
  data_file <- find_data_file()

  # Load model data when server starts
  message("Loading model data from: ", data_file)
  tryCatch({
    loaded_data <<- readRDS(data_file)
    message("Model data loaded successfully")
  }, error = function(e) {
    message("Error loading model data: ", e$message)
    stop("Failed to load model data. Please check the file path and try again.")
  })

  # Set up CORS
  pr$registerHook("preroute", function(req) {
    if (req$REQUEST_METHOD == "OPTIONS") {
      res <- list(
        status = 200,
        body = "",
        headers = list(
          "Access-Control-Allow-Origin" = "*",
          "Access-Control-Allow-Methods" = "GET, POST, PUT, DELETE, OPTIONS",
          "Access-Control-Allow-Headers" = "Content-Type",
          "Access-Control-Max-Age" = "86400"
        )
      )
      res
    }
    NULL
  })

  pr$registerHook("postserialize", function(req, res) {
    res$headers[["Access-Control-Allow-Origin"]] <- "*"
    res
  })
}

#* Get API information
#* @get /
function() {
  list(
    apiName = "Churn Prediction API",
    version = "1.0.0",
    endpoints = list(
      "/model/info" = "Get model information",
      "/model/predictions" = "Get model predictions",
      "/model/predictions/{id}" = "Get prediction for specific customer",
      "/model/risk-groups" = "Get churn by risk groups",
      "/model/overall-churn" = "Get overall churn statistics"
    )
  )
}

#* Get model information
#* @get /model/info
function() {
  list(
    totalCustomers = nrow(loaded_data$raw_data),
    churnRate = paste0(round(mean(loaded_data$raw_data$Churn == "Yes") * 100, 2), "%"),
    importantVariables = loaded_data$vars$importance %>%
      head(5) %>%
      select(variable, percentage) %>%
      as.list()
  )
}

#* Get model predictions with optional filtering
#* @param limit The maximum number of records to return (use "all" for all records)
#* @param riskgroup Filter by risk group (1-10)
#* @param haschurned Filter by churn status (Yes/No)
#* @get /model/predictions
function(limit = "100", riskgroup = "", haschurned = "") {
  result <- loaded_data$predictions

  # Apply filters if provided
  if (!is.null(riskgroup) && riskgroup != "") {
    result <- result %>% filter(RiskGroup == riskgroup)
  }

  if (!is.null(haschurned) && haschurned != "") {
    result <- result %>% filter(Churn == haschurned)
  }

  # Limit the number of results unless "all" is specified
  if (tolower(limit) == "all") {
    # Return all results
    result %>%
      select(customerID, Churn, Predict, PredictProbability, RiskGroup,
             tenure, Contract, MonthlyCharges, TotalCharges)
  } else {
    # Apply limit - convert to numeric, default to 100 if not a valid number
    limit_num <- tryCatch({
      as.numeric(limit)
    }, error = function(e) {
      100  # Default to 100 if conversion fails
    })

    if (is.na(limit_num) || limit_num <= 0) {
      limit_num <- 100  # Default to 100 if invalid
    }

    # Apply limit
    result %>%
      head(limit_num) %>%
      select(customerID, Churn, Predict, PredictProbability, RiskGroup,
             tenure, Contract, MonthlyCharges, TotalCharges)
  }
}

#* Get prediction for a specific customer
#* @param id The customer ID
#* @get /model/predictions/<id:character>
function(id = "") {
  # Return error if ID is empty
  if (id == "") {
    return(list(error = "Customer ID is required"))
  }

  customer <- loaded_data$predictions %>%
    filter(customerID == id)

  if (nrow(customer) == 0) {
    list(error = "Customer not found")
  } else {
    customer
  }
}

#* Get churn by risk groups
#* @get /model/risk-groups
function() {
  loaded_data$churn_by_risk_groups
}

#* Get overall churn statistics
#* @get /model/overall-churn
function() {
  loaded_data$overall_churn
}

#* Get financial impact of churn by risk group
#* @get /model/financial-impact
function() {
  loaded_data$charge_for_risk_groups
}

#* Get all model predictions (not limited)
#* @get /model/all-predictions
function() {
  loaded_data$predictions
}
