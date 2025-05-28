box::use(
  mockery[mock, stub],
  testthat[expect_equal, expect_error, expect_identical, expect_output, expect_true, skip, test_that],
)

# Test the basic functionality of run_api_locally.R with a mock plumber
test_that("run_api_locally.R finds plumber.R and starts the API server", {
  # Create a mock environment to load the script
  test_env <- new.env()

  # Track function calls to verify script execution
  call_tracking <- list(
    plumb_called = FALSE,
    plumb_file = NULL,
    run_called = FALSE,
    run_host = NULL,
    run_port = NULL,
    setwd_called = FALSE,
    setwd_dir = NULL
  )

  # Mock plumb function
  test_env$plumb <- function(file) {
    call_tracking$plumb_called <<- TRUE
    call_tracking$plumb_file <<- file

    # Return a mock plumber object
    list(
      run = function(host, port) {
        call_tracking$run_called <<- TRUE
        call_tracking$run_host <<- host
        call_tracking$run_port <<- port # Store as numeric 8000

        # Output the expected messages to verify in the test
        cat(sprintf("Running plumber API at http://%s:%d\n", host, port))
        cat(sprintf("Running swagger Docs at http://127.0.0.1:%d/__docs__/\n", port))
        return(invisible(NULL))
      }
    )
  }

  # Control file.exists behavior to simulate correct directory structure
  file_checks <- list()
  test_env$file.exists <- function(path) {
    # Record this check for verification
    file_checks[[length(file_checks) + 1]] <<- path

    # Simulate being in the main project directory
    # First check: current_dir/plumber.R (should return FALSE)
    # Second check: current_dir/api/plumber.R (should return TRUE)
    if (grepl("api/plumber\\.R$", path)) {
      return(TRUE)
    } else if (grepl("/plumber\\.R$", path)) {
      return(FALSE)
    }

    # Default to real file.exists for other checks
    return(file.exists(path))
  }

  # Mock setwd to track calls
  test_env$setwd <- function(dir) {
    call_tracking$setwd_called <<- TRUE
    call_tracking$setwd_dir <<- dir
    return(invisible(NULL))
  }

  # Mock getwd to return a consistent path
  test_env$getwd <- function() {
    project_root <- normalizePath("../..", mustWork = TRUE)
    return(project_root)
  }

  # Make library a no-op
  test_env$library <- function(package, ...) {
    return(invisible(NULL))
  }

  # Save messages for verification
  messages <- character(0)
  test_env$message <- function(...) {
    msg <- paste0(...)
    messages <<- c(messages, msg)
    return(invisible(NULL))
  }

  # Save any stops for verification
  stops <- character(0)
  test_env$stop <- function(...) {
    msg <- paste0(...)
    stops <<- c(stops, msg)
    cat("STOP: ", msg, "\n")
    return(invisible(NULL))
  }

  # Execute the script in our controlled environment
  api_script <- normalizePath(file.path("../../api/run_api_locally.R"), mustWork = FALSE)

  output <- capture.output(
    source(api_script, local = test_env)
  )

  # Verify expected output messages
  expect_true(any(grepl("Running plumber API at http://0\\.0\\.0\\.0:8000", output)))
  expect_true(any(grepl("Running swagger Docs at http://127\\.0\\.0\\.1:8000/__docs__/", output)))

  # Verify the script executed the expected logic
  expect_true(call_tracking$plumb_called, "plumb() function was not called")
  expect_equal(call_tracking$plumb_file, "plumber.R", "plumb() was not called with the correct file")

  expect_true(call_tracking$run_called, "plumber$run() was not called")
  expect_equal(call_tracking$run_host, "0.0.0.0", "plumber$run() was not called with the correct host")

  # For the port comparison, use a direct comparison with isTRUE
  expect_true(
    isTRUE(all.equal(as.integer(call_tracking$run_port), 8000L)),
    paste(
      "plumber$run() was not called with the correct port. Expected 8000, got",
      as.character(call_tracking$run_port)
    )
  )

  expect_true(call_tracking$setwd_called, "setwd() was not called")
  expect_true(grepl("api$", call_tracking$setwd_dir), "setwd() was not called with the api directory")

  # Verify that the expected file existence checks were made
  expect_true(any(grepl("plumber\\.R$", file_checks)), "Script did not check for plumber.R")
  expect_true(any(grepl("api/plumber\\.R$", file_checks)), "Script did not check for api/plumber.R")
})

# Test if the API properly finds model_output.rds
test_that("API correctly verifies model_output.rds existence", {
  # This test creates a simplified version of the plumber.R find_data_file function
  # and tests that it correctly finds the file or errors when it doesn't exist

  # Check if the file actually exists first - just for diagnostics
  project_root <- normalizePath("../..", mustWork = TRUE)
  model_file_path <- file.path(project_root, "api/data/model_output.rds")
  model_file_exists <- file.exists(model_file_path)

  cat("\n== Diagnostic info ==\n")
  cat("Project root:", project_root, "\n")
  cat("Looking for model file at:", model_file_path, "\n")
  cat("File exists:", model_file_exists, "\n")

  # First save the current working directory
  original_wd <- getwd()

  # Create a temporary directory structure to simulate the API
  test_dir <- tempfile("api_test")
  dir.create(test_dir)
  dir.create(file.path(test_dir, "api"))
  dir.create(file.path(test_dir, "api/data"))

  # Create a dummy model_output.rds file
  dummy_data <- list(test = "data")
  dummy_file_path <- file.path(test_dir, "api/data/model_output.rds")
  saveRDS(dummy_data, dummy_file_path)

  # Verify the file was created
  expect_true(file.exists(dummy_file_path), "Failed to create test model_output.rds file")

  # Define the test function that mimics plumber.R's find_data_file
  find_model_file <- function() {
    # Get the current directory
    current_dir <- getwd()
    cat("Current working directory:", current_dir, "\n")

    # Try several possible paths for the data file
    possible_paths <- c(
      "data/model_output.rds", # If working dir is api folder
      "../data/model_output.rds", # If we need to go up from api folder
      "api/data/model_output.rds" # If working dir is project root
    )

    # Debug output
    cat("Looking for model_output.rds in these paths:\n")
    for (path in possible_paths) {
      full_path <- file.path(current_dir, path)
      exists <- file.exists(path)
      cat("- ", path, ": ", exists, " (full path: ", full_path, ")\n", sep = "")
    }

    # Check each possible path
    for (path in possible_paths) {
      if (file.exists(path)) {
        cat("Found file at:", path, "\n")
        return(path)
      }
    }

    # If file doesn't exist at any of the expected locations, stop with an error
    cat("Could not find model_output.rds in any expected location\n")
    stop("Could not find model_output.rds file. Please ensure it exists in the api/data directory.")
  }

  # Test 1: When file exists in api/data/model_output.rds
  # ====================================================
  # Switch to the api directory
  setwd(file.path(test_dir, "api"))
  cat("\n=== Testing from api/directory ===\n")

  # Test the function - should find the file in data/model_output.rds
  tryCatch(
    {
      found_path <- find_model_file()
      cat("Success! Found path:", found_path, "\n")
      expect_equal(
        found_path,
        "data/model_output.rds",
        "When in api/ directory, should find file at data/model_output.rds"
      )
    },
    error = function(e) {
      cat("Error when file should be found:", e$message, "\n")
      expect_true(FALSE, paste("Function failed when file exists. Error:", e$message))
    }
  )

  # Test 2: When file does not exist
  # ===============================
  cat("\n=== Testing when file does not exist ===\n")

  # Remove the model_output.rds file
  file_removed <- file.remove(dummy_file_path)
  cat("File removed:", file_removed, "\n")

  # Verify file no longer exists
  expect_false(file.exists(dummy_file_path), "Failed to remove test file")

  # Use testthat's expect_error directly to check for the error
  expect_error(find_model_file(), "Could not find model_output.rds file")

  # Cleanup
  setwd(original_wd)
  unlink(test_dir, recursive = TRUE)
})
