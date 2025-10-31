# Deployment script for Manual Practice Beta
library(rsconnect)

setwd("/Users/mak/Library/CloudStorage/GoogleDrive-kriegsman.1@gmail.com/My Drive/ExperiMEntal/Manual_Practice_Beta")
list.files()
# Should show: app.R, config.R, schema files, json key path in config

rsconnect::setAccountInfo(
  name = "l9edvk-michael0a0kriegsman",
  token = "90419EA8AC4926D6A563788108EB996A",
  secret = "qNfuHtYXHyodNmxAPHX6g5Nz1M1f4gqu4bTSDGc6"
)

# Update existing app instead of creating new (you're at max apps)
rsconnect::deployApp(
  appDir = ".",
  appName = "experimental_app",  # Using existing app name
  account = "l9edvk-michael0a0kriegsman",
  forceUpdate = TRUE
)

