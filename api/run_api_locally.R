library(plumber)

# Simple approach to find plumber.R
current_dir <- getwd()
api_dir <- current_dir

# If we're in the main project directory, add api/
if (!file.exists(file.path(current_dir, "plumber.R")) &&
      file.exists(file.path(current_dir, "api", "plumber.R"))) {
  api_dir <- file.path(current_dir, "api")
  plumber_path <- file.path(api_dir, "plumber.R")
} else {
  # Either we're already in api/ or need to use the current directory
  plumber_path <- file.path(current_dir, "plumber.R")
}

# Make sure the file exists
if (!file.exists(plumber_path)) {
  stop("Cannot find plumber.R file. Please run this script from either:\n",
       "  1. The main project directory (with 'Rscript api/run_api.R')\n",
       "  2. The api/ directory (with 'Rscript run_api.R')")
}

# Set working directory to wherever plumber.R is located
# This ensures relative paths within plumber.R work correctly
setwd(dirname(plumber_path))

message("Starting Plumber API using: ", plumber_path)
pr <- plumb("plumber.R")
pr$run(host = "0.0.0.0", port = 8000)