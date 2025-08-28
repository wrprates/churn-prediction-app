box::use(
  R6[R6Class],
  rlang[inform],
)

box::use(
  app / logic / load_data,
)

#' R6 class for managing churn data
#' @export
churn_data_store <- R6Class(
  "ChurnDataStore",
  public = list(
    # Fields
    data = NULL,
    is_loaded = FALSE,

    # Initialize
    initialize = function() {
      inform("Initializing ChurnDataStore R6 class")
    },

    # Load data if needed and return it
    get_data = function(force_reload = FALSE, csv_file_path = NULL) {
      if (force_reload || is.null(self$data)) {
        inform("Loading data into R6 store")
        if (!is.null(csv_file_path)) {
          self$process_csv(csv_file_path)
        } else {
          self$load_data()
        }
      } else {
        inform("Using cached data from R6 store")
      }
      return(self$data)
    },

    # Load data from source (api or local)
    load_data = function() {
      self$data <- tryCatch(
        {
          inform("Calling load_data function")
          loaded_data <- load_data$load_data()
          inform("Data loaded successfully into R6 store")
          self$is_loaded <- TRUE
          loaded_data
        },
        error = function(e) {
          inform(paste("Error loading data in R6 store:", e$message))
          # Return a minimal data structure with empty defaults
          self$is_loaded <- FALSE
          list(
            raw_data = data.frame(),
            predictions = data.frame(),
            overall_churn = data.frame(),
            vars = list(
              importance = data.frame(
                variable = character(0),
                percentage = numeric(0)
              )
            ),
            colors = c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
          )
        }
      )
      invisible(self)
    },

    # Process CSV file and load data
    process_csv = function(csv_file_path) {
      self$data <- tryCatch(
        {
          inform("Processing CSV file via R6 store")
          loaded_data <- load_data$load_data(csv_file_path = csv_file_path)
          if (!is.null(loaded_data)) {
            inform("CSV data processed successfully into R6 store")
            self$is_loaded <- TRUE
            loaded_data
          } else {
            inform("CSV processing failed, falling back to default load")
            self$load_data()
            self$data
          }
        },
        error = function(e) {
          inform(paste("Error processing CSV in R6 store:", e$message))
          # Return a minimal data structure with empty defaults
          self$is_loaded <- FALSE
          list(
            raw_data = data.frame(),
            predictions = data.frame(),
            overall_churn = data.frame(),
            vars = list(
              importance = data.frame(
                variable = character(0),
                percentage = numeric(0)
              )
            ),
            colors = c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")
          )
        }
      )
      invisible(self)
    }
  )
)

# Create a singleton instance to be used across the app
#' @export
data_store <- churn_data_store$new()
