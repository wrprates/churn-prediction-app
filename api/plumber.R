# Self-contained Plumber API for Churn Prediction
# Combines functionality from both plumber.R and api.R into a single file

library(plumber)
library(dplyr)
library(h2o)
library(tibble)

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
        return(path)
      }
    }

    # If file doesn't exist at any of the expected locations, stop with an error
    stop(
      paste0("Could not find ", filename, " file. Please ensure it exists in the api/data directory.")
    )
  }

  # Get the correct path to the model data and model file
  data_file <- find_file("model_output.rds")
  model_file <- find_file("churn_model.h2o")

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

  # Load the saved H2O model
  message("Loading H2O model from: ", model_file)
  tryCatch(
    {
      # Get the directory part of the model_file path
      model_dir <- dirname(model_file)
      model_name <- basename(model_file)
      # Remove .h2o extension if present
      model_name <- sub("\\.h2o$", "", model_name)

      # Load the model
      churn_model <<- h2o.loadModel(path = paste0(model_dir, "/", model_name))
      message("H2O model loaded successfully")
    },
    error = function(e) {
      message("Error loading H2O model: ", e$message)
      message("API will continue with pre-computed predictions only")
    }
  )

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

  # Convert the input data to a data frame
  tryCatch(
    {
      # If customer_data is a list but not a data frame, convert it
      if (is.list(customer_data) && !is.data.frame(customer_data)) {
        # If it's a single customer (list of values), convert to a single-row data frame
        if (!any(sapply(customer_data, is.list))) {
          customer_df <- as.data.frame(t(unlist(customer_data)), stringsAsFactors = FALSE)
        } else {
          # If it's a list of customers, use do.call to bind them
          customer_df <- do.call(
            rbind,
            lapply(customer_data, function(x) {
              as.data.frame(x, stringsAsFactors = FALSE)
            })
          )
        }
      } else if (is.data.frame(customer_data)) {
        customer_df <- customer_data
      } else {
        return(list(error = "Invalid customer data format"))
      }

      # Convert categorical variables to factors
      for (col in names(customer_df)) {
        if (is.character(customer_df[[col]])) {
          customer_df[[col]] <- as.factor(customer_df[[col]])
        }
      }

      # Convert to H2O frame for prediction
      h2o_frame <- as.h2o(customer_df)

      # Make predictions
      predictions <- h2o.predict(churn_model, h2o_frame)

      # Process the predictions
      result <- customer_df |>
        tibble::as_tibble() |>
        bind_cols(
          as_tibble(predictions) |>
            select(Predict = predict, PredictProbability = Yes) |>
            mutate(PredictProbability = round(100 * PredictProbability, 2))
        ) |>
        mutate(RiskGroup = as.factor(11 - ntile(PredictProbability, 10))) |>
        arrange(desc(PredictProbability))

      return(result)
    },
    error = function(e) {
      return(list(error = paste("Error making predictions:", e$message)))
    }
  )
}
