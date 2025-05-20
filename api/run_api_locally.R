library(plumber)

# Function to check if model data exists
check_model_data <- function() {
  # Get the current directory
  current_dir <- getwd()

  # Define potential file paths for model data and model
  possible_data_paths <- c(
    file.path(current_dir, "data", "model_output.rds"),
    file.path(current_dir, "..", "api", "data", "model_output.rds"),
    file.path(current_dir, "api", "data", "model_output.rds")
  )

  possible_model_paths <- c(
    file.path(current_dir, "data", "churn_model.h2o"),
    file.path(current_dir, "..", "api", "data", "churn_model.h2o"),
    file.path(current_dir, "api", "data", "churn_model.h2o")
  )

  # Check if files exist
  data_exists <- any(sapply(possible_data_paths, file.exists))
  model_exists <- any(sapply(possible_model_paths, file.exists))

  # If either file is missing, run data processing
  if (!data_exists || !model_exists) {
    message("Model data or model file not found. Running data processing script...")

    # Determine project root directory to source data_processing.R
    if (file.exists(file.path(current_dir, "scripts", "data_processing.R"))) {
      # We're in project root
      script_path <- file.path(current_dir, "scripts", "data_processing.R")
    } else if (file.exists(file.path(current_dir, "..", "scripts", "data_processing.R"))) {
      # We're in api/ directory
      script_path <- file.path(current_dir, "..", "scripts", "data_processing.R")
    } else {
      stop("Cannot find data_processing.R script. Please ensure it exists in the scripts/ directory.")
    }

    message("Sourcing data processing script: ", script_path)
    source(script_path)
    return(TRUE)
  }

  message("Model data and model file found.")
  return(TRUE)
}

# Simple approach to find plumber.R
current_dir <- getwd()
api_dir <- current_dir

# If we're in the main project directory, add api/
if (
  !file.exists(file.path(current_dir, "plumber.R")) &&
    file.exists(file.path(current_dir, "api", "plumber.R"))
) {
  api_dir <- file.path(current_dir, "api")
  plumber_path <- file.path(api_dir, "plumber.R")
} else {
  # Either we're already in api/ or need to use the current directory
  plumber_path <- file.path(current_dir, "plumber.R")
}

# Make sure the file exists
if (!file.exists(plumber_path)) {
  stop(
    "Cannot find plumber.R file. Please run this script from either:\n",
    "  1. The main project directory (with 'Rscript api/run_api_locally.R')\n",
    "  2. The api/ directory (with 'Rscript run_api_locally.R')"
  )
}

# Check if model data exists and create if needed
if (!check_model_data()) {
  stop("Failed to ensure model data exists")
}

# Set working directory to wherever plumber.R is located
# This ensures relative paths within plumber.R work correctly
setwd(dirname(plumber_path))

# Process command line arguments for port
args <- commandArgs(trailingOnly = TRUE)
port <- 8000 # Default port
if (length(args) > 0) {
  port_arg <- as.numeric(args[1])
  if (!is.na(port_arg) && port_arg > 0) {
    port <- port_arg
    message("Using port: ", port)
  }
}

message("Starting Plumber API using: ", plumber_path)
pr <- plumb(plumber_path)
pr$run(host = "0.0.0.0", port = port)
