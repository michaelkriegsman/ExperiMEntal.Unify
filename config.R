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
# Set MPB_GS_SERVICE_JSON to an absolute path to your service account JSON to enable non-interactive access
GS_SERVICE_JSON <- Sys.getenv(
  "MPB_GS_SERVICE_JSON",
  unset = "/Users/mak/Library/CloudStorage/GoogleDrive-kriegsman.1@gmail.com/My Drive/ExperiMEntal/ExperiMEntal Backend/ExperiMEntal/august-storm-464819-q7-c26ff636dc17.json"
)


