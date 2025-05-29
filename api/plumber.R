# Self-contained Plumber API for Churn Prediction
library(plumber)
library(dplyr)
library(h2o)
library(tibble)

# Initialize H2O
h2o.init()

# Source our data processing functions
source("data_processing_functions.R")

# Initialize model variable
churn_model <- NULL

#* @apiTitle Churn Prediction API
#* @apiDescription API for predicting customer churn
#* @apiVersion 1.0.0

# Plumber router function
#* @plumber
function(pr) {
  # Try to load the H2O model
  script_dir <- getwd()
  h2o_model_dir <- file.path(script_dir, "data", "churn_model")

  if (dir.exists(h2o_model_dir)) {
    tryCatch(
      {
        model_files <- list.files(h2o_model_dir, pattern = "\\.h2o$", full.names = TRUE)
        if (length(model_files) > 0) {
          churn_model <<- h2o.loadModel(path = model_files[1])
          message("H2O model loaded successfully")
        }
      },
      error = function(e) {
        message("Error loading H2O model: ", e$message)
      }
    )
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
#* @tag "Api Information"
#* @get /
function() {
  list(
    apiName = "Churn Prediction API",
    version = "1.0.0",
    endpoints = list(
      "/predict" = "Predict churn for a single customer (POST)",
      "/predict_batch" = "Predict churn for multiple customers (POST)"
    ),
    modelStatus = if (is.null(churn_model)) "Not loaded" else "Loaded and ready for predictions"
  )
}

#* Predict churn for a single customer
#* @tag Prediction
#* @param customerData A JSON object with customer data
#* @post /predict
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please ensure the model exists in data/churn_model directory."))
  }

  # Parse the request body
  customer_data <- req$body

  # Validate the input data
  if (is.null(customer_data) || length(customer_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Process and predict
  tryCatch(
    {
      # Convert to H2O frame
      customer_h2o <- as.h2o(customer_data)

      # Make prediction
      prediction <- h2o.predict(churn_model, customer_h2o)

      # Convert prediction to list
      pred_list <- as.list(prediction)

      # Calculate risk group based on probability
      prob <- pred_list$Yes
      risk_group <- ceiling(prob * 10) # 1-10 scale

      return(list(
        prediction = pred_list$predict,
        probability = prob,
        risk_group = risk_group
      ))
    },
    error = function(e) {
      return(list(error = paste("Error making prediction:", e$message)))
    }
  )
}

#* Predict churn for multiple customers
#* @tag Prediction
#* @param customerData A JSON array of customer data objects
#* @post /predict_batch
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please ensure the model exists in data/churn_model directory."))
  }

  # Parse the request body
  customers_data <- req$body

  # Validate the input data
  if (is.null(customers_data) || length(customers_data) == 0) {
    return(list(error = "No customer data provided"))
  }

  # Ensure we have a list of customers
  if (!is.list(customers_data)) {
    return(list(error = "Input must be an array of customer data objects"))
  }

  # Process each customer
  results <- list()
  errors <- list()

  for (i in seq_along(customers_data)) {
    tryCatch(
      {
        # Convert to H2O frame
        customer_h2o <- as.h2o(customers_data[[i]])

        # Make prediction
        prediction <- h2o.predict(churn_model, customer_h2o)

        # Convert prediction to list
        pred_list <- as.list(prediction)

        # Calculate risk group based on probability
        prob <- pred_list$Yes
        risk_group <- ceiling(prob * 10) # 1-10 scale

        # Add to results
        results[[i]] <- list(
          index = i,
          prediction = pred_list$predict,
          probability = prob,
          risk_group = risk_group
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
