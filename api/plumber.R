# Self-contained Plumber API for Churn Prediction
library(plumber)
library(dplyr)
library(tidyr)
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
      "/predict_batch" = "Predict churn for multiple customers (POST)",
      "/process_csv" = "Process CSV data and return full analytics (POST)"
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
      list(error = paste("Error making prediction:", e$message))
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
  list(
    results = results,
    errors = errors,
    total_processed = length(results),
    total_errors = length(errors)
  )
}

#* Process CSV data and return full analytics
#* @tag "CSV Processing"
#* @param csvData JSON object containing CSV data as array of records
#* @post /process_csv
function(req) {
  # Check if model is loaded
  if (is.null(churn_model)) {
    return(list(error = "Model not loaded. Please ensure the model exists in data/churn_model directory."))
  }

  # Parse the request body
  csv_data <- req$body

  # Validate the input data
  if (is.null(csv_data) || length(csv_data) == 0) {
    return(list(error = "No CSV data provided"))
  }

  # Convert JSON to data frame
  tryCatch(
    {
      # If csvData is a list of records, convert to data frame
      if (is.list(csv_data) && length(csv_data) > 0) {
        # Check if it's array of objects or single object
        if (is.list(csv_data[[1]])) {
          # Array of objects - convert each to data frame row
          raw_data <- do.call(rbind, lapply(csv_data, function(row) {
            as.data.frame(row, stringsAsFactors = FALSE)
          }))
        } else {
          # Single object - convert directly
          raw_data <- as.data.frame(csv_data, stringsAsFactors = FALSE)
        }
      } else {
        return(list(error = "CSV data must be an array of objects"))
      }

      # Validate required columns
      required_cols <- c(
        "customerID", "gender", "SeniorCitizen", "Partner", "Dependents",
        "tenure", "PhoneService", "MultipleLines", "InternetService",
        "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
        "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling",
        "PaymentMethod", "MonthlyCharges", "TotalCharges"
      )

      missing_cols <- setdiff(required_cols, names(raw_data))
      if (length(missing_cols) > 0) {
        return(list(error = paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
      }

      # Data preprocessing
      # Convert numeric columns
      numeric_cols <- c("tenure", "MonthlyCharges", "TotalCharges", "SeniorCitizen")
      for (col in numeric_cols) {
        raw_data[[col]] <- as.numeric(raw_data[[col]])
      }

      # Handle missing TotalCharges
      raw_data$TotalCharges[is.na(raw_data$TotalCharges)] <- raw_data$MonthlyCharges[is.na(raw_data$TotalCharges)]

      # Convert categorical columns to factors
      categorical_cols <- setdiff(names(raw_data), c(numeric_cols, "customerID"))
      for (col in categorical_cols) {
        raw_data[[col]] <- as.factor(raw_data[[col]])
      }

      # Make predictions
      message("Converting to H2O frame...")
      data_h2o <- as.h2o(raw_data)

      message("Making predictions...")
      predictions_h2o <- h2o.predict(churn_model, data_h2o)
      predictions_df <- as.data.frame(predictions_h2o)

      # Combine with original data
      predictions_full <- cbind(raw_data, predictions_df)

      # Rename H2O columns to match frontend expectations
      names(predictions_full)[names(predictions_full) == "predict"] <- "Predict"
      names(predictions_full)[names(predictions_full) == "Yes"] <- "PredictProbability"

      # Calculate risk groups and add Churn column
      predictions_full$RiskGroup <- ceiling(predictions_full$PredictProbability * 10)
      predictions_full$RiskGroup[predictions_full$RiskGroup == 0] <- 1
      predictions_full$Churn <- ifelse(predictions_full$Predict == "Yes", "Yes", "No")

      # Create comprehensive analytics data structure

      # 1. Raw data and predictions
      result_data <- list()
      result_data$raw_data <- raw_data
      result_data$predictions <- predictions_full

      # 2. Overall churn statistics
      churn_summary <- predictions_full %>%
        group_by(Churn) %>%
        summarise(
          Count = n(),
          Percentage = round(n() / nrow(predictions_full) * 100, 1),
          .groups = "drop"
        ) %>%
        mutate(Customer = ifelse(Churn == "Yes", "Churners", "Non-Churners"))

      result_data$overall_churn <- churn_summary

      # 3. Churn by risk groups - in long format for UI compatibility
      risk_group_long <- predictions_full %>%
        group_by(RiskGroup, Churn) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(RiskGroup) %>%
        mutate(
          total = sum(count),
          prop = round(count / sum(count) * 100, 1)
        ) %>%
        arrange(RiskGroup, Churn) %>%
        ungroup()
      
      # Add cumulative proportion for "Yes" churn
      risk_group_long <- risk_group_long %>%
        group_by(Churn) %>%
        mutate(cum_prop = ifelse(Churn == "Yes", cumsum(prop), NA)) %>%
        ungroup()

      result_data$churn_by_risk_groups <- risk_group_long

      # 4. Financial impact by risk groups - with Churn breakdown for UI
      financial_impact <- predictions_full %>%
        group_by(RiskGroup, Churn) %>%
        summarise(
          CustomerCount = n(),
          AvgMonthlyCharges = round(mean(MonthlyCharges, na.rm = TRUE), 2),
          SumMonthlyCharges = round(sum(MonthlyCharges, na.rm = TRUE), 2),
          .groups = "drop"
        )

      result_data$charge_for_risk_groups <- financial_impact

      # 5. Variable importance (static for now, can be enhanced)
      result_data$vars <- list(
        importance = data.frame(
          variable = c("Contract", "tenure", "MonthlyCharges", "InternetService", "OnlineSecurity"),
          percentage = c(0.45, 0.30, 0.12, 0.08, 0.05)
        )
      )

      # 6. Color scheme
      result_data$colors <- c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")

      # Return the complete analytics package
      return(result_data)
    },
    error = function(e) {
      list(error = paste("Error processing CSV data:", e$message))
    }
  )
}
