# Self-contained Plumber API for Churn Prediction
# Combines functionality from both plumber.R and api.R into a single file

library(plumber)
library(dplyr)
library(h2o)
library(tibble)

# Source our data processing functions
source("data_processing_functions.R")

# Initialize data and model variables
loaded_data <- NULL
churn_model <- NULL

#* @apiTitle Churn Prediction API
#* @apiDescription API for accessing churn prediction model data - GitHub: [https://github.com/wrprates/churn-prediction-app](https://github.com/wrprates/churn-prediction-app)
#* @apiVersion 1.1.0

# Plumber router function
#* @plumber
function(pr) {
  # Initialize H2O
  h2o.init()

  # Find model and data files in the api/data directory
  find_file <- function(filename) {
    # Get the current directory
    script_dir <- getwd()
    message("Current working directory: ", script_dir)

    # Try several possible paths for the file
    possible_paths <- c(
      paste0("data/", filename), # If working dir is api folder
      paste0("../data/", filename), # If we need to go up from api folder
      paste0("api/data/", filename) # If working dir is project root
    )

    # Check each possible path
    for (path in possible_paths) {
      message("Checking path: ", path)
      if (file.exists(path)) {
        message("Found file at: ", path)
        # Return absolute path to avoid any confusion
        return(normalizePath(path))
      }
    }

    # If file doesn't exist at any of the expected locations, stop with an error
    stop(
      paste0("Could not find ", filename, " file. Please ensure it exists in the api/data directory.")
    )
  }

  # Get the correct path to the model data file
  data_file <- find_file("model_output.rds")

  # Load model data when server starts
  message("Loading model data from: ", data_file)
  tryCatch(
    {
      loaded_data <<- readRDS(data_file)

      # Display simple data summary
      message("Model data loaded successfully")
      message("Dataset summary:")
      message("- loaded_data$raw_data rows: ", nrow(loaded_data$raw_data))
      message("- loaded_data$predictions rows: ", nrow(loaded_data$predictions))
    },
    error = function(e) {
      message("Error loading model data: ", e$message)
      stop(
        "Failed to load model data. Please check the file path and try again."
      )
    }
  )

  # Try multiple methods to load the H2O model
  model_loaded <- FALSE

  # Method 1: Check if there's a GBM model file in the churn_model directory
  script_dir <- getwd()
  h2o_model_dir <- file.path(script_dir, "data", "churn_model")

  # Check for H2O model directory
  if (dir.exists(h2o_model_dir)) {
    message("Found H2O model directory at: ", h2o_model_dir)

    # List all files in the directory
    model_files <- list.files(h2o_model_dir, pattern = "^GBM_model_.*", full.names = TRUE)
    message("Found ", length(model_files), " GBM model files in directory")

    if (length(model_files) > 0) {
      # Try to load the first GBM model file
      tryCatch(
        {
          message("Trying to load model from: ", model_files[1])
          churn_model <<- h2o.loadModel(path = model_files[1])
          message("H2O model loaded successfully from GBM file")
          model_loaded <- TRUE
        },
        error = function(e) {
          message("Error loading H2O model from GBM file: ", e$message)
        }
      )
    }
  }

  # Method 2: Try to load using the .h2o file directly
  if (!model_loaded) {
    tryCatch(
      {
        h2o_file <- find_file("churn_model.h2o")
        message("Trying to load model directly from .h2o file: ", h2o_file)
        churn_model <<- h2o.importModel(h2o_file)
        message("H2O model loaded successfully from .h2o file")
        model_loaded <- TRUE
      },
      error = function(e) {
        message("Error loading H2O model from .h2o file: ", e$message)
      }
    )
  }

  # Method 3: Try the whole directory as a last resort
  if (!model_loaded && dir.exists(h2o_model_dir)) {
    tryCatch(
      {
        message("Trying to load model from directory: ", h2o_model_dir)
        churn_model <<- h2o.loadModel(h2o_model_dir)
        message("H2O model loaded successfully from directory")
        model_loaded <- TRUE
      },
      error = function(e) {
        message("Error loading H2O model from directory: ", e$message)
        message("API will continue with pre-computed predictions only")
      }
    )
  }

  if (!model_loaded) {
    message("All model loading attempts failed. API will run with pre-computed predictions only.")
  }

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
    version = "1.1.0",
    dataStructure = "The raw data contains all 7043 customers, while predictions are only available for the test set
    (2109 customers, about 30% of the full dataset)",
    endpoints = list(
      "/model/info" = "Get model information",
      "/model/predictions" = "Get model predictions",
      "/model/predictions/{id}" = "Get prediction for specific customer",
      "/model/predict" = "Make predictions on new customer data (POST)",
      "/model/process-and-predict" = "Process and predict new customer data (POST)",
      "/model/validate-data" = "Validate customer data structure (POST)",
      "/model/risk-groups" = "Get churn by risk groups",
      "/model/overall-churn" = "Get overall churn statistics",
      "/model/financial-impact" = "Get financial impact data by risk group",
      "/model/all-predictions" = "Get all model predictions without limits",
      "/model/colors" = "Get color palette for charts",
      "/model/raw-data" = "Get all raw data (all customers)"
    ),
    modelStatus = if (is.null(churn_model)) "Not loaded" else "Loaded and ready for predictions"
  )
}

#* Get model information
#* @get /model/info
function() {
  # Get the importance data frame without limiting to top 5
  importance_df <- loaded_data$vars$importance

  # Create a list of variable names and percentages that will be properly structured
  variable_importance <- list(
    variable = importance_df$variable,
    percentage = importance_df$percentage
  )

  # Log the customer count for debugging
  total_customers <- nrow(loaded_data$raw_data)
  total_predictions <- nrow(loaded_data$predictions)
  message("Reporting total customers in /model/info: ", total_customers)
  message("Total predictions available: ", total_predictions)

  # Explain the difference in customer counts
  if (total_customers != total_predictions) {
    message(
      "Note: raw_data contains all customers (",
      total_customers,
      "), while predictions contains only test set customers (",
      total_predictions,
      ") - approximately 30% of the full dataset"
    )
  }

  list(
    totalCustomers = total_customers,
    predictionsAvailable = total_predictions,
    churnRate = paste0(
      round(mean(loaded_data$raw_data$Churn == "Yes") * 100, 2),
      "%"
    ),
    importantVariables = variable_importance,
    datasetExplanation = "The raw data contains all customers,
    while predictions are only available for the test set (about 30% of customers)",
    datasetSummary = list(
      raw_data_rows = total_customers,
      predictions_rows = total_predictions
    )
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
    result <- result |> filter(RiskGroup == riskgroup)
  }

  if (!is.null(haschurned) && haschurned != "") {
    result <- result |> filter(Churn == haschurned)
  }

  # Limit the number of results unless "all" is specified
  if (tolower(limit) == "all") {
    # Return all results with logging to help troubleshoot
    message("Predictions endpoint with limit=all. Total rows: ", nrow(result))

    # Ensure no unexpected filtering occurs
    if (
      nrow(result) < nrow(loaded_data$predictions) &&
        is.null(riskgroup) &&
        riskgroup == "" &&
        is.null(haschurned) &&
        haschurned == ""
    ) {
      message(
        "WARNING: Data size mismatch when limit=all. Expected ",
        nrow(loaded_data$predictions),
        " but got ",
        nrow(result)
      )
    }

    result |>
      select(
        customerID,
        Churn,
        Predict,
        PredictProbability,
        RiskGroup,
        tenure,
        Contract,
        MonthlyCharges,
        TotalCharges
      )
  } else {
    # Apply limit - convert to numeric, default to 100 if not a valid number
    limit_num <- tryCatch(
      {
        as.numeric(limit)
      },
      error = function(e) {
        100 # Default to 100 if conversion fails
      }
    )

    if (is.na(limit_num) || limit_num <= 0) {
      limit_num <- 100 # Default to 100 if invalid
    }

    # Apply limit
    message(
      "Predictions endpoint with limit=",
      limit_num,
      ". Total rows before limit: ",
      nrow(result)
    )
    result |>
      head(limit_num) |>
      select(
        customerID,
        Churn,
        Predict,
        PredictProbability,
        RiskGroup,
        tenure,
        Contract,
        MonthlyCharges,
        TotalCharges
      )
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

  customer <- loaded_data$predictions |>
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
  # Ensure we return the full dataset without any filtering or limitations
  message(
    "Serving all-predictions endpoint. Total rows: ",
    nrow(loaded_data$predictions)
  )
  message(
    "Note: This returns predictions for test data only (approximately 30% of full dataset)"
  )

  # Verify data integrity before returning
  if (is.null(loaded_data$predictions) || nrow(loaded_data$predictions) == 0) {
    message("WARNING: Predictions data is empty or NULL")
  }

  loaded_data$predictions
}

#* Get color palette for charts
#* @get /model/colors
function() {
  if (!is.null(loaded_data$colors)) {
    loaded_data$colors
  } else {
    # Default colors if not available
    c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
  }
}

#* Get all raw data (all customers)
#* @get /model/raw-data
function() {
  # This returns the complete dataset including customers without predictions
  message("Serving raw-data endpoint. Total rows: ", nrow(loaded_data$raw_data))

  # Verify data integrity before returning
  if (is.null(loaded_data$raw_data) || nrow(loaded_data$raw_data) == 0) {
    message("WARNING: Raw data is empty or NULL")
  }

  loaded_data$raw_data
}

#* Validate customer data structure
#* @param customerData A JSON object with customer data
#* @post /model/validate-data
function(req) {
  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Use our validation function
  validation_result <- validate_customer_data(customer_data)
  return(validation_result)
}

#* Process and predict customer data
#* @param customerData A JSON object with customer data
#* @post /model/process-and-predict
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please initialize the API with a valid model."))
  }

  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Validate data structure
  validation_result <- validate_customer_data(customer_data)
  if (!validation_result$valid) {
    return(validation_result)
  }

  # Process the data
  tryCatch(
    {
      # Process the data
      processed_data <- process_customer_data(customer_data)

      # Make predictions
      predictions <- make_predictions(churn_model, processed_data)

      # Calculate risk metrics
      risk_metrics <- calculate_risk_metrics(predictions)

      # Return results
      return(list(
        predictions = predictions,
        risk_metrics = risk_metrics
      ))
    },
    error = function(e) {
      return(list(error = paste("Error processing data:", e$message)))
    }
  )
}

#* Make predictions on new customer data
#* @param customerData A JSON object with customer data
#* @post /model/predict
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please initialize the API with a valid model."))
  }

  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Validate data structure
  validation_result <- validate_customer_data(customer_data)
  if (!validation_result$valid) {
    return(validation_result)
  }

  # Process and predict
  tryCatch(
    {
      # Process the data
      processed_data <- process_customer_data(customer_data)

      # Make predictions
      predictions <- make_predictions(churn_model, processed_data)

      return(predictions)
    },
    error = function(e) {
      return(list(error = paste("Error making predictions:", e$message)))
    }
  )
}

#* Preprocess customer data without making predictions
#* @param customerData A JSON object with customer data
#* @post /model/preprocess
function(req) {
  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Validate data structure
  validation_result <- validate_customer_data(customer_data)
  if (!validation_result$valid) {
    return(validation_result)
  }

  # Process the data
  tryCatch(
    {
      # Process the data
      processed_data <- process_customer_data(customer_data)
      return(list(
        processed_data = processed_data,
        message = "Data preprocessed successfully"
      ))
    },
    error = function(e) {
      return(list(error = paste("Error preprocessing data:", e$message)))
    }
  )
}

#* Calculate risk metrics for predictions
#* @param predictions A JSON object with prediction results
#* @post /model/calculate-risk
function(req) {
  # Parse the request body
  predictions <- req$body

  # Validate the input data
  if (is.null(predictions) || length(predictions) == 0) {
    return(list(error = "No prediction data provided"))
  }

  # Calculate risk metrics
  tryCatch(
    {
      risk_metrics <- calculate_risk_metrics(predictions)
      return(list(
        risk_metrics = risk_metrics,
        message = "Risk metrics calculated successfully"
      ))
    },
    error = function(e) {
      return(list(error = paste("Error calculating risk metrics:", e$message)))
    }
  )
}

#* Process and predict multiple customers in batch
#* @param customerData A JSON array of customer data objects
#* @post /model/batch-predict
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please initialize the API with a valid model."))
  }

  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Ensure we have a list of customers
  if (!is.list(customer_data)) {
    return(list(error = "Input must be an array of customer data objects"))
  }

  # If it's a single customer, wrap it in a list
  if (!any(sapply(customer_data, is.list))) {
    customer_data <- list(customer_data)
  }

  # Process each customer
  results <- list()
  errors <- list()

  for (i in seq_along(customer_data)) {
    tryCatch(
      {
        # Validate individual customer data
        validation_result <- validate_customer_data(customer_data[[i]])
        if (!validation_result$valid) {
          errors[[i]] <- list(
            index = i,
            error = validation_result$errors
          )
          next
        }

        # Process the data
        processed_data <- process_customer_data(customer_data[[i]])

        # Make predictions
        predictions <- make_predictions(churn_model, processed_data)

        # Calculate risk metrics
        risk_metrics <- calculate_risk_metrics(predictions)

        # Add to results
        results[[i]] <- list(
          index = i,
          predictions = predictions,
          risk_metrics = risk_metrics
        )
      },
      error = function(e) {
        errors[[i]] <- list(
          index = i,
          error = e$message
        )
      }
    )
  }

  # Return combined results
  return(list(
    results = results,
    errors = errors,
    total_processed = length(results),
    total_errors = length(errors)
  ))
}

#* Get processing pipeline status
#* @get /model/pipeline-status
function() {
  list(
    model_loaded = !is.null(churn_model),
    data_loaded = !is.null(loaded_data),
    h2o_initialized = h2o.clusterIsUp(),
    available_endpoints = list(
      "/model/validate-data" = "Validate customer data structure",
      "/model/preprocess" = "Preprocess customer data",
      "/model/predict" = "Make predictions on new customer data",
      "/model/process-and-predict" = "Process and predict new customer data",
      "/model/calculate-risk" = "Calculate risk metrics for predictions",
      "/model/batch-predict" = "Process and predict multiple customers in batch"
    )
  )
}
