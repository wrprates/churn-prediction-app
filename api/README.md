# Churn Prediction API

This folder contains a RESTful API built with Plumber that exposes data from the churn prediction model.

## Running the API

To start the API server locally:

```bash
# From the project root
Rscript -e "library(plumber); pr <- plumb('api/plumber.R'); pr$run(port=8000)"
```

or with R:
```r
library(plumber)
pr <- plumb("api/plumber.R")
pr$run(port=8000)
```

This will start the API server on port 8000.

## Available Endpoints

### Root Endpoint

- `GET /`: Returns API information and available endpoints

### Model Data Endpoints

- `GET /model/info`: Returns basic model information (total customers, churn rate, important variables)
- `GET /model/predictions`: Returns model predictions with optional filtering
  - Parameters:
    - `limit`: Maximum number of records to return (default: 100, use "all" to get all records)
    - `riskgroup`: Filter by risk group (1-10)
    - `haschurned`: Filter by churn status ("Yes"/"No")
- `GET /model/all-predictions`: Returns ALL model predictions without any limits
- `GET /model/predictions/<id>`: Returns prediction for a specific customer ID
- `GET /model/risk-groups`: Returns churn statistics by risk groups
- `GET /model/overall-churn`: Returns overall churn statistics
- `GET /model/financial-impact`: Returns financial impact data by risk group

## Example Usage

```bash
# Get API information
curl http://localhost:8000/

# Get model information
curl http://localhost:8000/model/info

# Get predictions for high-risk customers who churned
curl http://localhost:8000/model/predictions?riskgroup=1&haschurned=Yes

# Get ALL predictions without limit
curl http://localhost:8000/model/predictions?limit=all

# Get ALL predictions using the dedicated endpoint
curl http://localhost:8000/model/all-predictions

# Get prediction for a specific customer
curl http://localhost:8000/model/predictions/7590-VHVEG
```

## Integration with Shiny

To use this API in your Shiny app, you can make HTTP requests to fetch the data:

```r
# In your Shiny app server.R or app.R
library(httr)
library(jsonlite)

# Get all predictions
response <- GET("http://localhost:8000/model/all-predictions")
predictions_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Then use the data in your Shiny app
# ...
```

## Deploying to DigitalOcean

To deploy this API to a DigitalOcean droplet:

### Using plumberDeploy

```r
library(plumberDeploy)
library(analogsea)

# Provision a new droplet (only needed once)
droplet_id <- plumberDeploy::do_provision(example = FALSE)

# Install required packages on the droplet
analogsea::install_r_package(droplet_id, c("dplyr", "plumber"))

# Deploy your API to the droplet
plumberDeploy::do_deploy_api(
  droplet = droplet_id,
  path = "api",
  localPath = "./api",
  port = 8000,
  docs = TRUE,
  overwrite = TRUE
)
```

After deployment, your API will be available at:
```
http://your-droplet-ip:8000/
```

## Dependencies

Required R packages:
- plumber
- dplyr

For deployment:
- plumberDeploy
- analogsea

Install with:
```r
install.packages(c("plumber", "dplyr", "plumberDeploy", "analogsea"))
```
