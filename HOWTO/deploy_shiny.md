# HOWTO: Deploy a Shiny App (shinyapps.io)

This is the repeatable path to deploy any module (e.g., `Manual_Practice_Beta/`).

## Prereqs
- R + RStudio
- A shinyapps.io account
- `rsconnect` package installed
- The Google Sheet shared with your service account (Editor), and `MPB_GS_SERVICE_JSON` points to that JSON key on deploy

## One-time setup
```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "<your_account>",
  token  = "<your_token>",
  secret = "<your_secret>"
)
```

## Deploy
From the repo root (or the module directory):
```r
rsconnect::deployApp("Manual_Practice_Beta", forceUpdate = TRUE)
```

## Environment/config
- The app reads `config.R` in the module. For non-interactive access, set env vars on shinyapps.io:
  - `MPB_SHEET_ID` — Google Sheet ID
  - `MPB_GS_SERVICE_JSON` — path to the service account JSON inside the bundle (if you include it), or use a secure alternative (e.g., s3)
  - `MPB_DEFAULT_USER_ID`, `MPB_TIMEZONE`, etc.

## Sharing
- Share the Sheet with the service account email in the JSON (`client_email`)
- If Google Calendar is enabled, share the target calendar with the same service account (Make changes to events)

## Troubleshooting
- If the app deploys but can’t read the sheet, check sharing + env vars
- If auth prompts appear, ensure the JSON is present and the path resolves on the server
- Use `rsconnect::showLogs(appName = "<app>")` for server logs
