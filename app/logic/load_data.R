box::use(
  config[get],
  httr[content, GET, status_code],
  jsonlite[fromJSON],
  utils[capture.output, head, str],
)

#' @export
load_data <- function(use_api = NULL) {
  # Load configuration
  config <- get()

  # Determine whether to use API (priority to function argument)
  use_api_config <- if (!is.null(use_api)) {
    use_api
  } else {
    config$use_api
  }

  if (use_api_config) {
    tryCatch(
      {
        # Try to load data from API
        message("Loading data from API...")

        # Get base URL from configuration
        api_base_url <- config$api_url

        # Complete data structure to be returned
        data <- list()

        # Function to make API request
        fetch_api_data <- function(endpoint) {
          url <- paste0(api_base_url, endpoint)
          message("Fetching data from: ", url)

          response <- tryCatch(
            {
              GET(url)
            },
            error = function(e) {
              message("Error connecting to API at ", url, ": ", e$message)
              NULL
            }
          )

          if (is.null(response)) {
            return(NULL)
          }

          if (status_code(response) != 200) {
            message(
              "Failed to access endpoint ",
              endpoint,
              ": status code ",
              status_code(response)
            )
            return(NULL)
          }

          tryCatch(
            {
              result <- fromJSON(content(response, "text", encoding = "UTF-8"))
              # Debug the returned data
              if (!is.null(result)) {
                if (is.data.frame(result)) {
                  message("  Retrieved ", nrow(result), " rows of data")
                  if (nrow(result) > 0) {
                    message("  First row ID: ", head(result[, 1], 1))
                  }
                } else {
                  message("  Data type: ", class(result))
                }
              }
              result
            },
            error = function(e) {
              message("Error parsing JSON from API: ", e$message)
              NULL
            }
          )
        }

        # Fetch model info first - it has the total customer count we need
        model_info <- fetch_api_data("/api/model/info")
        if (is.null(model_info)) {
          message(
            "Failed to get model info from API. Continuing to fetch other endpoints..."
          )
          # Create a default model_info for compatibility
          model_info <- list(
            totalCustomers = 0,
            churnRate = "0%",
            importantVariables = NULL
          )
        }

        # Debug the model_info structure safely
        message("API model_info structure:")
        if (!is.null(model_info$totalCustomers)) {
          total_customers <- as.numeric(model_info$totalCustomers[1]) # Extract the numeric value from array
          message("- totalCustomers: ", total_customers)
        } else {
          message("- totalCustomers: NULL")
          total_customers <- 0
        }

        message(
          "- churnRate: ",
          if (!is.null(model_info$churnRate)) paste0(model_info$churnRate) else "NULL"
        )

        # Store important variables info
        if (!is.null(model_info$importantVariables)) {
          data$vars <- list(
            importance = data.frame(
              variable = model_info$importantVariables$variable,
              percentage = as.numeric(model_info$importantVariables$percentage)
            )
          )
        } else {
          # Fallback to default values if not available
          message("Using default variable importance values.")
          data$vars <- list(
            importance = data.frame(
              variable = c(
                "Contract",
                "tenure",
                "MonthlyCharges",
                "InternetService",
                "OnlineSecurity"
              ),
              percentage = c(0.45, 0.30, 0.12, 0.08, 0.05)
            )
          )
        }

        # Try alternative API endpoints if the main one doesn't return full data
        # First try the all-predictions endpoint (which should have all data)
        full_predictions <- fetch_api_data("/api/model/all-predictions")

        # Also get the raw data which contains ALL customers
        raw_data <- fetch_api_data("/api/model/raw-data")

        # Check if we have raw data
        if (!is.null(raw_data) && nrow(raw_data) > 0) {
          message(
            "Successfully retrieved raw_data with ",
            nrow(raw_data),
            " rows"
          )
          data$raw_data <- raw_data
        } else {
          message("Failed to get raw_data, will use predictions data instead")
        }

        # Check if we got a reasonable number of rows for predictions
        if (
          is.null(full_predictions) ||
            (total_customers > 0 && !is.null(raw_data) && nrow(full_predictions) < nrow(raw_data) * 0.2)
        ) {
          message(
            "all-predictions endpoint returned incomplete data. Trying predictions endpoint with limit=all..."
          )

          # Try the regular predictions endpoint with limit=all
          full_predictions_alt <- fetch_api_data(
            "/api/model/predictions?limit=all"
          )

          if (
            !is.null(full_predictions_alt) &&
              (is.null(full_predictions) || nrow(full_predictions_alt) > nrow(full_predictions))
          ) {
            message("Using data from /api/model/predictions?limit=all instead")
            full_predictions <- full_predictions_alt
          }
        }

        if (is.null(full_predictions) || nrow(full_predictions) == 0) {
          message("Failed to get predictions data from API.")
          # Don't return NULL here, continue with empty data that can be filled later
          full_predictions <- data.frame()
        }

        # Store the predictions
        data$predictions <- full_predictions

        # Make sure raw_data exists - if we didn't get it from /raw-data endpoint, use predictions
        if (is.null(data$raw_data)) {
          message(
            "Using predictions as raw_data (some customers may be missing)"
          )
          data$raw_data <- full_predictions
        }

        # Debug the dataset size
        message("Dataset sizes:")
        message("- full_predictions rows: ", nrow(full_predictions))
        message("- data$predictions rows: ", nrow(data$predictions))
        message("- data$raw_data rows: ", nrow(data$raw_data))

        # Verify if we have the correct number of customers
        if (total_customers > 0) {
          actual_count <- nrow(data$raw_data)
          actual_predictions <- nrow(data$predictions)

          message("Customer count check:")
          message("- Expected total customers from API: ", total_customers)
          message("- Actual customers in dataset: ", actual_count)
          message("- Actual predictions in dataset: ", actual_predictions)

          # It's normal for predictions to be only about 30% of total customers
          # But raw_data should contain all or most customers
          if (actual_count < total_customers * 0.9) {
            message(
              "Significant customer count mismatch: ",
              "Expected about ",
              total_customers,
              " from /model/info, ",
              "but got only ",
              actual_count,
              " in raw_data"
            )

            # At this point, if we're significantly under the expected count,
            # it's better to fall back to the local file which should have full data
            message(
              "API returned incomplete raw_data. Falling back to local file..."
            )
            return(NULL)
          } else {
            message("Customer count acceptable: ", actual_count)

            # Warn about predictions if they're too few
            if (actual_predictions < total_customers * 0.25) {
              message(
                "WARNING: Predictions count is lower than expected (",
                actual_predictions,
                " vs expected ~",
                round(total_customers * 0.3),
                "). This is acceptable but may affect some visualizations."
              )
            }
          }
        }

        # Fetch remaining data
        endpoints <- list(
          churn_by_risk_groups = "/api/model/risk-groups",
          overall_churn = "/api/model/overall-churn",
          charge_for_risk_groups = "/api/model/financial-impact",
          colors = "/api/model/colors"
        )

        # Fetch remaining data
        for (name in names(endpoints)) {
          data[[name]] <- fetch_api_data(endpoints[[name]])
          if (is.null(data[[name]])) {
            message("Failed to get ", name, " from API.")
            # Create empty placeholder for this component
            data[[name]] <- if (name == "colors") {
              c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5") # Default colors
            } else {
              data.frame() # Empty data frame for other components
            }
          }
        }

        # Check if we have valid data to proceed
        if (nrow(data$raw_data) > 0) {
          message("Final dataset summary:")
          message("- raw_data rows: ", nrow(data$raw_data))
          message("- predictions rows: ", nrow(data$predictions))
          message("Data loaded successfully from API.")
          return(data)
        } else {
          message("API data incomplete or invalid. Trying local file...")
        }
      },
      error = function(e) {
        message("Error accessing API: ", e$message, ". Trying local file...")
      }
    )
  } else {
    message("API usage disabled in configuration. Using local file...")
  }

  # If we get here, we'll use the local file
  message("Loading data from local file...")

  # Check multiple possible cache paths
  possible_cache_paths <- c(
    "data/model_output.rds", # Standard path
    "./data/model_output.rds", # With explicit current dir
    "../data/model_output.rds", # Up one level
    # Only use the API directory as a last resort
    "api/model_output.rds", # In API directory
    "./api/model_output.rds", # API with explicit current dir
    "model_output.rds" # Directly in current dir
  )

  # Try to find a valid file
  cache_path <- NULL
  for (path in possible_cache_paths) {
    message("Checking for model data at: ", path)
    if (file.exists(path)) {
      message("Found model data at: ", path)
      cache_path <- path
      break
    }
  }

  if (!is.null(cache_path)) {
    # Load the .rds file
    tryCatch(
      {
        message("Reading RDS file from: ", cache_path)
        data <- readRDS(cache_path)

        # Debug the loaded data
        message("RDS data structure:")
        message("- Names in data: ", paste(names(data), collapse = ", "))

        if ("raw_data" %in% names(data)) {
          message(
            "- raw_data dimensions: ",
            nrow(data$raw_data),
            " x ",
            ncol(data$raw_data)
          )
          if (nrow(data$raw_data) > 0) {
            message(
              "- First few IDs: ",
              paste(head(data$raw_data[, 1], 3), collapse = ", ")
            )
          }
        } else {
          message("WARNING: raw_data not found in loaded RDS file")
          # Create raw_data from predictions if available
          if (
            "predictions" %in%
              names(data) &&
              is.data.frame(data$predictions) &&
              nrow(data$predictions) > 0
          ) {
            message("Creating raw_data from predictions data")
            data$raw_data <- data$predictions
          } else {
            message(
              "WARNING: Cannot create raw_data, no predictions data available"
            )
          }
        }

        message("Data loaded successfully from local file.")
        message(
          "- raw_data rows: ",
          if ("raw_data" %in% names(data)) nrow(data$raw_data) else "not found"
        )
        message(
          "- predictions rows: ",
          if ("predictions" %in% names(data)) nrow(data$predictions) else "not found"
        )

        return(data)
      },
      error = function(e) {
        message("Error reading RDS file: ", e$message)
        # Continue to the next path
        NULL
      }
    )
  }

  # If we get here, we couldn't load from any file
  message("ERROR: Could not find or load model_output.rds from any location")
  message("Current working directory: ", getwd())
  message("Files in current directory:")
  files <- list.files(".", recursive = FALSE)
  message(paste(files, collapse = ", "))
  message("Files in ./data directory:")
  if (dir.exists("data")) {
    files <- list.files("data", recursive = FALSE)
    message(paste(files, collapse = ", "))
  } else {
    message("data directory not found")
  }

  # Return a minimal default data structure
  message("Creating minimal default data structure")
  default_data <- list(
    raw_data = data.frame(
      customerID = character(0),
      Churn = character(0),
      MonthlyCharges = numeric(0),
      Contract = character(0)
    ),
    predictions = data.frame(
      customerID = character(0),
      Churn = character(0),
      PredictProbability = numeric(0),
      RiskGroup = character(0)
    ),
    overall_churn = data.frame(
      Churn = character(0),
      `Count Customers` = numeric(0),
      `% Customers` = numeric(0),
      Customer = character(0)
    ),
    churn_by_risk_groups = data.frame(),
    charge_for_risk_groups = data.frame(),
    vars = list(
      importance = data.frame(
        variable = c(
          "Contract",
          "tenure",
          "MonthlyCharges",
          "InternetService",
          "OnlineSecurity"
        ),
        percentage = c(0.45, 0.30, 0.12, 0.08, 0.05)
      )
    ),
    colors = c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
  )

  return(default_data)
}
