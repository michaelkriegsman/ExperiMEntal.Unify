# Configuration for Manual Practice Beta

# TODO: set these before running
SHEET_ID <- Sys.getenv("MPB_SHEET_ID", unset = "1QfHPiVyOVk5XrFloDKznoivLHMTtBI5F0fY0TsgykuE")
DEFAULT_USER_ID <- Sys.getenv("MPB_DEFAULT_USER_ID", unset = "Michael.Test")
DEFAULT_CALENDAR_ID <- Sys.getenv("MPB_DEFAULT_CALENDAR_ID", unset = "")
TIMEZONE <- Sys.getenv("MPB_TIMEZONE", unset = "America/New_York")
DEMO_USER_ID <- Sys.getenv("MPB_DEMO_USER_ID", unset = "Demo")

# Sheet tab names
TAB_USERS <- "users"
TAB_ROUTINES <- "routines"
TAB_STEPS <- "routine_steps"
TAB_ENTRIES <- "practice_entries"

# Optional service account auth for Google Sheets
# For local: absolute path works
# For shinyapps.io: set MPB_GS_SERVICE_JSON env var pointing to a file inside the app bundle
# For now, try relative path first, then fall back to env var or absolute path
GS_SERVICE_JSON <- Sys.getenv("MPB_GS_SERVICE_JSON", unset = "")
if (!nzchar(GS_SERVICE_JSON)) {
  # Try relative path from app directory
  rel_path <- "august-storm-464819-q7-c26ff636dc17.json"
  abs_path <- "/Users/mak/Library/CloudStorage/GoogleDrive-kriegsman.1@gmail.com/My Drive/ExperiMEntal/ExperiMEntal Backend/ExperiMEntal/august-storm-464819-q7-c26ff636dc17.json"
  if (file.exists(rel_path)) {
    GS_SERVICE_JSON <- normalizePath(rel_path)
  } else if (file.exists(abs_path)) {
    GS_SERVICE_JSON <- abs_path
  }
}


