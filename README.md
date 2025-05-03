# Churn Prediction App

Customer churn prediction and analysis application based on machine learning models.

## Description

This Shiny application provides a user-friendly interface for analyzing customer churn prediction data, allowing users to identify high-risk customers, churn factors, and potential financial impact.

## Live Demo

A live version of this application is deployed and available at:

**[https://statsights-churn-prediction-app.share.connect.posit.cloud](https://statsights-churn-prediction-app.share.connect.posit.cloud)**

You can explore the full functionality without setting up your own instance.

## Features

- Company churn overview
- Detailed predictive model analysis
- Customer risk analysis
- Financial impact analysis

## Configuration

### Requirements

- R 4.2.0 or higher
- R packages listed in `dependencies.R`

### API Configuration

The application can work in two ways:

1. **API Mode**: Fetching data from a cloud-hosted REST API
2. **Local Mode**: Using local data file (fallback)

The API configuration is in the `config.yml` file:

```yaml
default:
  use_api: true
  api_url: "http://164.90.245.110"
```

You can modify these settings to control the application's behavior.

## Installation

Clone the repository:

```bash
git clone https://github.com/your-username/churn-prediction-app.git
cd churn-prediction-app
```

Install dependencies:

```r
renv::restore()
```

## Execution

To start the application:

```r
rhino::app()
```

## API

This application can use the churn prediction API available at `http://164.90.245.110`. The API provides the following endpoints:

- `GET /model/info`: Basic model information (important variables)
- `GET /model/predictions`: Model predictions with optional filtering (main endpoint used by the app)
  - Parameters:
    - `limit`: Maximum number of records to return (use "all" for all records)
    - `riskgroup`: Filter by risk group (1-10)
    - `haschurned`: Filter by churn status (Yes/No)

The application primarily uses the `/model/predictions?limit=all` endpoint to obtain complete data and calculate additional statistics locally.


## Project Structure

```
.
├── app/                  # Shiny application
│   ├── logic/            # Application logic
│   ├── view/             # Interface components
│   └── main.R            # App entry point
├── api/                  # Plumber API
│   └── plumber.R         # API implementation
├── data/                 # Model data
├── scripts/              # Processing scripts
└── config.yml            # Application settings
```

## License

MIT
