#' @export
load_data <- function() {
  # Check if cached data exists
  cache_path <- "data/model_output.rds"

  if (file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    stop("Error: data file 'data/model_output.rds' not found. Please run the data processing script first.")
  }
}
