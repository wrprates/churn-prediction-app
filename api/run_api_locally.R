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

  # Check for data file
  data_exists <- any(sapply(possible_data_paths, file.exists))

  # Check for model (either as .h2o file or directory)
  h2o_file_found <- FALSE
  h2o_dir_found <- FALSE
  h2o_file_path <- NULL

  # Check for .h2o file
  possible_h2o_file_paths <- c(
    file.path(current_dir, "data", "churn_model.h2o"),
    file.path(current_dir, "..", "api", "data", "churn_model.h2o"),
    file.path(current_dir, "api", "data", "churn_model.h2o")
  )

  for (path in possible_h2o_file_paths) {
    if (file.exists(path)) {
      h2o_file_found <- TRUE
      h2o_file_path <- path
      break
    }
  }

  # Check for model directory
  possible_model_dirs <- c(
    file.path(current_dir, "data", "churn_model"),
    file.path(current_dir, "..", "api", "data", "churn_model"),
    file.path(current_dir, "api", "data", "churn_model")
  )

  for (path in possible_model_dirs) {
    if (dir.exists(path)) {
      h2o_dir_found <- TRUE
      break
    }
  }

  # If we have the .h2o file but not the model directory, try to create model directory
  if (h2o_file_found && !h2o_dir_found && !is.null(h2o_file_path)) {
    message("Found .h2o file but no model directory. Creating model directory...")

    # Determine model directory path
    model_dir <- file.path(dirname(h2o_file_path), "churn_model")

    # Create the directory
    dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

    # Copy the .h2o file to the directory
    file.copy(h2o_file_path, file.path(model_dir, "churn_model"), overwrite = TRUE)

    message("Created model directory at: ", model_dir)
    return(TRUE)
  }

  # If either file is missing, run data processing
  if (!data_exists || (!h2o_file_found && !h2o_dir_found)) {
    message("Model data or model not found. Running data processing script...")

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

  message("Model data and model found.")
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
