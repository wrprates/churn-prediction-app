library(plumberDeploy)
library(analogsea)

# Your existing droplet ID
droplet_id <- 0000000000

# Install required packages on the droplet
analogsea::install_r_package(droplet_id, c("dplyr", "plumber"))

# Deploy your API to the droplet
plumberDeploy::do_deploy_api(
  droplet = droplet_id,                # Your droplet ID
  path = "api",                     # Remote path to deploy to
  localPath = "./api",                # Local path to your API files
  port = 8000,                         # Port to deploy on
  forward = TRUE,                     # Forward the port
  docs = TRUE,                          # Enable Swagger documentation
  overwrite = TRUE,                  # Overwrite existing deployment
)