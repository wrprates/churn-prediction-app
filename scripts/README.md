# Data Processing Scripts

This folder contains standalone scripts for the churn prediction model.

## Running the Scripts

You can run the data processing script directly:

```bash
# From the project root
Rscript scripts/data_processing.R
```

Or within R:

```r
# Set working directory to project root
setwd("/path/to/churn-prediction-app")

# Source the script - it will automatically run the processing
source("scripts/data_processing.R")

# The results are stored in the 'result' variable
# You can now use result directly:
summary(result$raw_data)
```

## Dependencies

Required R packages:
- dplyr
- h2o
- readr
- tibble

Install with:
```r
install.packages(c("dplyr", "h2o", "readr", "tibble"))
```
