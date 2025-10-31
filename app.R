suppressPackageStartupMessages({
  library(shiny)
  library(googlesheets4)
  library(dplyr)
  library(lubridate)
  library(later)
  library(shinyWidgets)
  # Using Calendar API directly via httr instead of googleCalendarR (package compatibility)
})

# Ensure a default CRAN mirror is set for install.packages to avoid prompt errors
if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] %in% c("@CRAN@", "", NA))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

source("config.R")

# Simple package check/install
ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Null-coalescing helper used throughout
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  x
}

invisible(lapply(c("shiny", "googlesheets4", "dplyr", "lubridate", "httr", "jsonlite", "shinyWidgets", "gargle", "curl"), ensure_pkg))

# Auth: prefer service account if available; otherwise deauth (public sheets only)
# Includes both Sheets and Calendar scopes for full functionality
GS_SCOPES <- c(
  "https://www.googleapis.com/auth/spreadsheets",
  "https://www.googleapis.com/auth/calendar"
)
if (nzchar(GS_SERVICE_JSON) && file.exists(GS_SERVICE_JSON)) {
  tryCatch({
    googlesheets4::gs4_auth(path = GS_SERVICE_JSON, scopes = GS_SCOPES)
  }, error = function(e) {
    message("GS auth via service account failed: ", e$message)
    googlesheets4::gs4_deauth()
  })
} else {
  googlesheets4::gs4_deauth()
}

# Data access helpers
read_sheet_safe <- function(sheet, range = NULL) {
  if (!nzchar(SHEET_ID)) {
    stop("SHEET_ID is not set")
  }
  tryCatch({
    if (is.null(range)) {
      googlesheets4::read_sheet(SHEET_ID, sheet = sheet)
    } else {
      googlesheets4::read_sheet(SHEET_ID, sheet = sheet, range = range)
    }
  }, error = function(e) {
    stop(sprintf("Failed to read sheet '%s': %s", sheet, e$message))
  })
}

write_append_entries <- function(df) {
  stopifnot(is.data.frame(df))
  googlesheets4::sheet_append(SHEET_ID, data = df, sheet = TAB_ENTRIES)
}

# Create Google Calendar event for completed practice session (using Calendar API directly via httr)
create_calendar_event <- function(entry_id, routine_name, started_at, ended_at, calendar_id, description = NULL) {
  message("create_calendar_event called with calendar_id='", calendar_id, "'")
  if (is.null(calendar_id) || !nzchar(calendar_id)) {
    message("No calendar ID configured, skipping calendar event creation")
    return(list(id = NULL, status = NA_integer_, body = NULL, error = "No calendar configured"))
  }
  
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    warning("httr/jsonlite not available; cannot create calendar event")
    return(list(id = NULL, status = NA_integer_, body = NULL, error = "Missing httr/jsonlite"))
  }
  
  tryCatch({
    if (!nzchar(GS_SERVICE_JSON) || !file.exists(GS_SERVICE_JSON)) {
      warning("Service account JSON not found; cannot create calendar event")
      return(list(id = NULL, status = NA_integer_, body = NULL, error = "Missing service account JSON"))
    }

    if (!requireNamespace("gargle", quietly = TRUE)) {
      warning("gargle package not available; cannot create calendar event")
      return(list(id = NULL, status = NA_integer_, body = NULL, error = "Missing gargle"))
    }

    token <- gargle::token_fetch(
      scopes = GS_SCOPES,
      path = GS_SERVICE_JSON
    )
    if (is.null(token)) {
      warning("Could not obtain access token for Calendar API")
      return(list(id = NULL, status = NA_integer_, body = NULL, error = "No access token"))
    }

    # Calendar API expects timestamps in the user's timezone for display
    # started_at and ended_at are already in user's timezone (passed from display_timestamp_local)
    # Extract timezone from the POSIXct object (set by display_timestamp_local)
    user_tz_for_cal <- attr(started_at, "tzone")
    if (is.null(user_tz_for_cal) || !nzchar(user_tz_for_cal)) {
      user_tz_for_cal <- TIMEZONE  # fallback to config
    }
    start_rfc <- format(started_at, "%Y-%m-%dT%H:%M:%S")
    end_rfc   <- format(ended_at,   "%Y-%m-%dT%H:%M:%S")

    url <- paste0("https://www.googleapis.com/calendar/v3/calendars/", curl::curl_escape(calendar_id), "/events")
    event_body <- list(
      summary = paste0(routine_name, " - Practice"),
      start = list(dateTime = start_rfc, timeZone = user_tz_for_cal),
      end   = list(dateTime = end_rfc,   timeZone = user_tz_for_cal),
      description = description %||% ""
    )

    auth_header <- paste("Bearer", token$credentials$access_token)
    resp <- httr::POST(
      url,
      httr::add_headers(Authorization = auth_header),
      httr::content_type_json(),
      body = jsonlite::toJSON(event_body, auto_unbox = TRUE)
    )

    sc <- httr::status_code(resp)
    body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "<no body>")
    if (sc >= 400) {
      warning(paste0("Calendar API error (", sc, "): ", body_txt))
      return(list(id = NULL, status = sc, body = body_txt, error = paste0("Calendar API error ", sc)))
    }

    parsed <- tryCatch(httr::content(resp, as = "parsed"), error = function(e) NULL)
    ev_id <- if (!is.null(parsed)) parsed$id else NULL
    if (is.null(ev_id)) {
      warning("Calendar event created but no event ID returned")
      return(list(id = NULL, status = sc, body = body_txt, error = "No event id"))
    }

    list(id = ev_id, status = sc, body = body_txt, error = NULL)
  }, error = function(e) {
    err_msg <- paste0("Failed to create calendar event: ", e$message, " (Calendar ID: ", calendar_id, ")")
    warning(err_msg)
    list(id = NULL, status = NA_integer_, body = NULL, error = err_msg)
  })
}

# Timezone helpers (UTC storage, user timezone display)
get_user_timezone <- function(user_id) {
  if (is.null(user_id) || !nzchar(user_id)) return(TIMEZONE)
  tryCatch({
    users <- read_sheet_safe(TAB_USERS)
    users$user_id_clean <- trimws(tolower(as.character(users$user_id)))
    user_id_clean <- trimws(tolower(as.character(user_id)))
    row <- users %>% dplyr::filter(.data$user_id_clean == user_id_clean) %>% dplyr::slice(1)
    if (nrow(row) > 0 && !is.null(row$timezone) && !is.na(row$timezone) && nzchar(as.character(row$timezone))) {
      return(as.character(row$timezone))
    }
    return(TIMEZONE)
  }, error = function(e) TIMEZONE)
}

# Store timestamp: convert local system time to UTC
store_timestamp_utc <- function(local_time = Sys.time()) {
  # Assume local_time is in system timezone, convert to UTC for storage
  lubridate::with_tz(local_time, tzone = "UTC")
}

# Display timestamp: convert UTC to user's timezone
display_timestamp_local <- function(utc_timestamp, user_id) {
  user_tz <- get_user_timezone(user_id)
  if (!inherits(utc_timestamp, c("POSIXct", "POSIXt"))) {
    utc_timestamp <- as.POSIXct(utc_timestamp, tz = "UTC")
  }
  lubridate::with_tz(utc_timestamp, tzone = user_tz)
}

# Format UTC timestamp for storage (ISO string, UTC)
format_utc_for_storage <- function(utc_timestamp) {
  if (!inherits(utc_timestamp, c("POSIXct", "POSIXt"))) {
    utc_timestamp <- as.POSIXct(utc_timestamp, tz = "UTC")
  }
  format(lubridate::with_tz(utc_timestamp, tzone = "UTC"), "%Y-%m-%d %H:%M:%S")
}

# Resolve calendar id for current user (sheet override > default)
resolve_user_calendar_id <- function(user_id) {
  message("  [resolve_user_calendar_id] Called with user_id='", user_id, "', type=", typeof(user_id))
  if (is.null(user_id) || !nzchar(user_id)) {
    message("  [resolve_user_calendar_id] user_id is null/empty, returning empty string")
    return("")
  }
  cal <- tryCatch({
    users <- read_sheet_safe(TAB_USERS)
    message("  [resolve_user_calendar_id] Read users sheet, got ", nrow(users), " rows")
    message("  [resolve_user_calendar_id] User IDs in sheet: ", paste(unique(users$user_id), collapse=", "))
    message("  [resolve_user_calendar_id] Columns in sheet: ", paste(names(users), collapse=", "))
    # Debug: show full contents of each row to spot any misalignment
    for (i in seq_len(min(5, nrow(users)))) {
      msg <- paste0("  [resolve_user_calendar_id] Row ", i, ": user_id='", users$user_id[i], 
                   "', google_calendar_id='", users$google_calendar_id[i], "'")
      message(msg)
    }
    
    # Trim whitespace and use case-insensitive matching
    # First, filter out rows with NA, empty, or literal "NA" string user_ids
    users_valid <- users[!is.na(users$user_id) & 
                         trimws(as.character(users$user_id)) != "" & 
                         trimws(tolower(as.character(users$user_id))) != "na", , drop = FALSE]
    message("  [resolve_user_calendar_id] After filtering invalid rows, got ", nrow(users_valid), " valid rows")
    
    users_valid$user_id_clean <- trimws(tolower(as.character(users_valid$user_id)))
    user_id_clean <- trimws(tolower(as.character(user_id)))
    message("  [resolve_user_calendar_id] Searching for user_id_clean='", user_id_clean, "'")
    
    # Debug: show all user_id_clean values
    message("  [resolve_user_calendar_id] All valid user_id_clean values: ", paste(users_valid$user_id_clean, collapse=", "))
    message("  [resolve_user_calendar_id] All valid original user_id values: ", paste(users_valid$user_id, collapse=", "))
    
    # Filter and verify the match is correct - only match on valid rows
    candidates <- users_valid[users_valid$user_id_clean == user_id_clean, , drop = FALSE]
    message("  [resolve_user_calendar_id] Candidates after filtering: ", nrow(candidates))
    if (nrow(candidates) > 0) {
      message("  [resolve_user_calendar_id] Candidate user_ids: ", paste(candidates$user_id, collapse=", "))
      message("  [resolve_user_calendar_id] Candidate user_id_clean values: ", paste(candidates$user_id_clean, collapse=", "))
    }
    
    # Get first row only if we have matches
    if (nrow(candidates) > 0) {
      row <- candidates[1, , drop = FALSE]
    } else {
      row <- candidates  # empty data frame
    }
    if (nrow(row) == 0) {
      message("  [resolve_user_calendar_id] User '", user_id, "' not found in users sheet. Available users: ", paste(unique(users_valid$user_id), collapse=", "))
      return("")
    }
    
    message("  [resolve_user_calendar_id] Found matching row!")
    message("  [resolve_user_calendar_id] Row user_id='", row$user_id, "', user_id_clean='", row$user_id_clean, "', google_calendar_id='", row$google_calendar_id, "'")
    
    # VERIFY THE MATCH IS CORRECT (double-check)
    row_user_id_clean <- trimws(tolower(as.character(row$user_id)))
    if (row_user_id_clean != user_id_clean) {
      message("  [resolve_user_calendar_id] ERROR: Row user_id mismatch! Expected '", user_id_clean, "' but got '", row_user_id_clean, "'")
      message("  [resolve_user_calendar_id] Aborting calendar lookup due to mismatch")
      return("")
    }
    
    val <- trimws(as.character(row$google_calendar_id))
    message("  [resolve_user_calendar_id] Trimmed calendar_id='", val, "', length=", length(val), ", nzchar=", nzchar(val))
    
    if (length(val) == 0 || is.na(val) || val == "" || !nzchar(val)) {
      message("  [resolve_user_calendar_id] User '", user_id, "' (row found) has empty google_calendar_id, skipping calendar event")
      return("")
    }
    message("  [resolve_user_calendar_id] RESOLVED - returning calendar '", val, "' for user '", user_id, "'")
    val
  }, error = function(e) {
    message("  [resolve_user_calendar_id] ERROR resolving calendar for user '", user_id, "': ", e$message)
    return("")
  })
  message("  [resolve_user_calendar_id] Final return value: '", cal, "'")
  cal
}

# Helper: convert 1-based column index to Excel letter(s)
col_index_to_letter <- function(idx) {
  letters <- c()
  while (idx > 0) {
    rem <- (idx - 1) %% 26
    letters <- c(LETTERS[rem + 1], letters)
    idx <- (idx - 1) %/% 26
  }
  paste0(letters, collapse = "")
}

# Update just ended_at cell for a matching row; prefer matching by started_at if provided
update_entry_ended_at <- function(entry_id, item_name, ended_at, started_at_hint = NULL) {
  tryCatch({
    entries <- read_sheet_safe(TAB_ENTRIES)

    # Check what columns actually exist and provide helpful error
    sheet_cols <- names(entries)
    required <- c("entry_id", "item_name", "started_at", "ended_at")
    missing <- required[!required %in% sheet_cols]
    if (length(missing) > 0) {
      stop(sprintf("Required columns missing in practice_entries sheet: %s. Found columns: %s", 
                   paste(missing, collapse=", "), paste(sheet_cols, collapse=", ")))
    }

    # Find candidate rows for this entry
    candidates <- which(entries$entry_id == entry_id & entries$item_name == item_name)
    if (length(candidates) == 0) {
      # This is expected on launch - don't show user-facing message
      return(FALSE)
    }

    # Prefer exact match on started_at if provided
    match_idx <- NA_integer_
    if (!is.null(started_at_hint)) {
      hint_str <- format(started_at_hint, "%Y-%m-%d %H:%M:%S")
      start_vals <- entries$started_at[candidates]
      start_strs <- tryCatch({
        format(lubridate::as_datetime(start_vals, tz = TIMEZONE), "%Y-%m-%d %H:%M:%S")
      }, error = function(e) {
        as.character(start_vals)
      })
      picked <- which(start_strs == hint_str)
      if (length(picked) > 0) match_idx <- candidates[picked[1]]
    }

    # Fallback: choose the latest row with empty ended_at
    if (is.na(match_idx) || is.null(match_idx)) {
      empty_ended <- candidates[which(is.na(entries$ended_at[candidates]) | entries$ended_at[candidates] == "")]
      if (length(empty_ended) == 0) return(FALSE)
      match_idx <- max(empty_ended)
    }

    # Find ended_at column index
    ended_col_idx <- which(names(entries) == "ended_at")
    if (length(ended_col_idx) != 1) stop("Could not locate ended_at column")
    
    row_num <- match_idx + 1  # +1 for header
    col_letter <- col_index_to_letter(ended_col_idx)
    target_range <- paste0(col_letter, row_num, ":", col_letter, row_num)
    # Store in UTC: ensure ended_at is converted to UTC
    if (!inherits(ended_at, c("POSIXct", "POSIXt"))) {
      ended_at <- as.POSIXct(ended_at, tz = "UTC")
    } else {
      ended_at <- lubridate::with_tz(ended_at, tzone = "UTC")
    }
    ended_at_str <- format_utc_for_storage(ended_at)

    # Write the single cell
    googlesheets4::range_write(
      SHEET_ID,
      sheet = TAB_ENTRIES,
      data = data.frame(ended_at = ended_at_str, stringsAsFactors = FALSE),
      range = target_range,
      col_names = FALSE
    )
    TRUE
  }, error = function(e) {
    stop(sprintf("Failed to update entry ended_at: %s", e$message))
  })
}

# Check for incomplete session
check_incomplete_session <- function(user_id) {
  if (is.null(user_id) || !nzchar(user_id)) return(NULL)
  tryCatch({
    entries <- read_sheet_safe(TAB_ENTRIES)
    if (nrow(entries) == 0) return(NULL)
    
    # Filter for this user and get latest entry
    user_entries <- entries %>%
      filter(.data$user_id == user_id) %>%
      arrange(desc(.data$created_at))
    
    if (nrow(user_entries) == 0) return(NULL)
    
    latest_entry <- user_entries[1, ]
    
    # Check if latest entry is "Complete and Save"
    if (!is.na(latest_entry$item_name) && latest_entry$item_name == "Complete and Save") {
      return(NULL)  # Session is complete
    }
    
    # Get all entries for this entry_group_id
    group_entries <- user_entries %>%
      filter(.data$entry_group_id == latest_entry$entry_group_id)
    
    # Find the Launch entry to get the actual started_at time
    launch_entry <- group_entries %>%
      filter(!is.na(.data$item_name) & .data$item_name == "Launch") %>%
      slice(1)
    
    started_at <- if (nrow(launch_entry) > 0) {
      launch_entry$started_at[1]
    } else {
      latest_entry$started_at
    }
    
    # Return the entry_group_id and routine_id for resume
    return(list(
      entry_group_id = latest_entry$entry_group_id,
      routine_id = latest_entry$routine_id,
      started_at = started_at,
      existing_entries = group_entries %>%
        filter(!is.na(.data$item_name) & .data$item_name != "Launch")
    ))
  }, error = function(e) {
    message("Error checking incomplete session: ", e$message)
    return(NULL)
  })
}

# Safe timestamp parser for mixed inputs from Sheets
safe_parse_time <- function(x, tz = TIMEZONE) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.POSIXct(x, tz = tz))
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = tz))
  x_chr <- suppressWarnings(as.character(x))
  try_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d")
  for (fmt in try_formats) {
    dt <- suppressWarnings(as.POSIXct(x_chr, format = fmt, tz = tz))
    if (!all(is.na(dt))) return(dt)
  }
  suppressWarnings(lubridate::ymd_hms(x_chr, tz = tz, quiet = TRUE))
}

ui <- fluidPage(
  titlePanel("Manual Practice Beta"),
  tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 1000px; margin: 0 auto; padding: 0 10px; }
      .main-panel { padding: 0 5px; }
      .sidebar-panel { padding-right: 10px; }
      .steps-container { width: 100%; min-height: auto; }
      .step-row { margin-bottom: 8px; }
      .step-name { font-weight: bold; padding-right: 10px !important; }
      .step-display-text { font-size: 0.9em; color: #666; font-style: italic; padding-top: 2px; padding-bottom: 4px; }
      .step-timer { text-align: right; padding-right: 10px !important; }
      .step-intended-duration { text-align: center; padding-right: 5px !important; color: #888; font-size: 0.9em; }
      .step-toggle { padding-left: 5px !important; }
      .routine-description { margin-top: 15px; margin-bottom: 15px; min-height: auto; max-height: 150px; padding: 8px; background-color: #f9f9f9; border-radius: 4px; }
      .col-sm-5, .col-sm-4, .col-sm-3, .col-sm-2 { padding-left: 5px; padding-right: 5px; }
      .switch-input { 
        min-width: 100px !important; 
        height: 38px !important; 
        border-radius: 19px !important;
      }
      .switch-input .switch-toggle { 
        width: 42px !important; 
        height: 34px !important; 
        border-radius: 17px !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2) !important;
      }
      .switch-input.on { background-color: #007bff !important; }
      .switch-input.off { background-color: #6c757d !important; }
      @media (max-width: 768px) {
        .switch-input { min-width: 90px !important; height: 36px !important; }
        .switch-input .switch-toggle { width: 40px !important; height: 32px !important; }
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$label("User"),
      textOutput("user_label"),
      uiOutput("resume_ui"),
      uiOutput("routine_select_ui"),
      actionButton("load_routine", "Launch Routine", class = "btn-primary"),
      tags$hr(),
      actionButton("complete_session", "Complete & Save", class = "btn-success"),
      tags$div(id = "save_status")
    ),
    mainPanel(
      width = 9,
      h4(textOutput("routine_title")),
      div(strong("Routine elapsed:"), textOutput("routine_elapsed", inline = TRUE)),
      div(class = "routine-description", textOutput("routine_description", inline = FALSE)),
      div(class = "steps-container", uiOutput("steps_ui"))
      # Diagnostics removed
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    user_id = DEFAULT_USER_ID,
    routines = NULL,
    steps = NULL,
    selected_routine = NULL,
    entry_id = NULL,            # switched from entry_group_id
    started_at = NULL,
    routine_timer_started = NULL,
    session_complete = FALSE,
    incomplete_session = NULL,
    resumed_session = FALSE,
    step_start_times = list(),
    step_frozen_elapsed = list()
  )

  # Read user from URL once: ?user=Michael.Test (fallback to DEFAULT_USER_ID)
  observeEvent(TRUE, {
    u <- tryCatch({
      q <- parseQueryString(isolate(session$clientData$url_search %||% ""))
      url_user <- q$user %||% DEFAULT_USER_ID
      message("=== USER INITIALIZATION ===")
      message("URL query string: ", isolate(session$clientData$url_search %||% ""))
      message("Parsed query: ", paste(names(q), "=", q, collapse=", "))
      message("q$user='", q$user, "'")
      message("DEFAULT_USER_ID='", DEFAULT_USER_ID, "'")
      message("Setting state$user_id='", url_user, "'")
      url_user
    }, error = function(e) {
      message("Error parsing URL query: ", e$message)
      message("Falling back to DEFAULT_USER_ID='", DEFAULT_USER_ID, "'")
      DEFAULT_USER_ID
    })
    state$user_id <- u
    message("state$user_id is now: '", state$user_id, "'")
    output$user_label <- renderText(if (is.null(u) || !nzchar(u)) "Guest" else u)
    
    # Check for incomplete session after user is set (disabled for now)
    # later(function() {
    #   tryCatch({
    #     incomplete <- check_incomplete_session(state$user_id)
    #     if (!is.null(incomplete)) {
    #       state$incomplete_session <- incomplete
    #     }
    #   }, error = function(e) {
    #     message("Could not check for incomplete session: ", e$message)
    #   })
    # }, delay = 0.5)
  }, once = TRUE)

  # Load routines for user
  load_routines <- reactive({
    req(state$user_id)
    df <- tryCatch({
      read_sheet_safe(TAB_ROUTINES)
    }, error = function(e) {
      showNotification(paste0("Error loading routines: ", e$message), type = "error")
      return(data.frame())
    })
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(status = as.character(status)) %>%
        filter(.data$user_id == state$user_id,
               is.na(.data$status) | .data$status %in% c("active", "")) %>%
        arrange(.data$routine_name)
    }
    state$routines <- df
    df
  })

  # Resume UI
  output$resume_ui <- renderUI({
    if (is.null(state$incomplete_session) || state$resumed_session) return(NULL)
    
    incomplete <- state$incomplete_session
    steps_completed <- if (is.data.frame(incomplete$existing_entries)) {
      nrow(incomplete$existing_entries)
    } else {
      0
    }
    
    tagList(
      tags$div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
        tags$strong("Incomplete Session Found"),
        tags$br(),
        tags$small(sprintf("You have an incomplete session with %d step(s) recorded.", steps_completed)),
        tags$br(),
        tags$br(),
        actionButton("resume_session", "Resume Session", class = "btn-warning btn-sm", style = "margin-right: 5px;"),
        actionButton("dismiss_resume", "Start New", class = "btn-default btn-sm")
      )
    )
  })

  output$routine_select_ui <- renderUI({
    df <- load_routines()
    if (!is.data.frame(df) || nrow(df) == 0) {
      # Do not auto-switch users; explain and show next steps
      sa_email <- tryCatch({
        if (nzchar(GS_SERVICE_JSON) && file.exists(GS_SERVICE_JSON)) {
          jsonlite::fromJSON(GS_SERVICE_JSON)$client_email %||% ""
        } else ""
      }, error = function(e) "")
      tagList(
        tags$div(style = "color:#c00;", "No active routines found or cannot read Google Sheet."),
        helpText(sprintf("Share the sheet with this service account (Editor): %s", ifelse(nchar(sa_email)>0, sa_email, "<service-account-email>"))),
        helpText(sprintf("Ensure 'routines' has an active row for user_id '%s' and 'routine_steps' has rows for its routine_id.", state$user_id))
      )
    } else {
      lbl <- function(row) paste0(row$routine_name, ifelse(is.na(row$time_of_day) || row$time_of_day=="", "", paste0(" (", row$time_of_day, ")")))
      choices <- setNames(df$routine_id, vapply(seq_len(nrow(df)), function(i) lbl(df[i,]), character(1)))
      selectInput("routine_id", "Routine", choices = choices)
    }
  })

  # Diagnostics removed

  # Resume session handler
  observeEvent(input$resume_session, {
    req(state$incomplete_session)
    incomplete <- state$incomplete_session
    
    state$selected_routine <- incomplete$routine_id
    state$entry_id <- incomplete$entry_group_id
    state$resumed_session <- TRUE
    
    # Load steps for the routine
    steps <- tryCatch({
      read_sheet_safe(TAB_STEPS) %>%
        filter(.data$routine_id == incomplete$routine_id) %>%
        arrange(.data$step_order)
    }, error = function(e) {
      showNotification(paste0("Error loading steps: ", e$message), type = "error")
      return(data.frame())
    })
    state$steps <- steps
    
    # Restore started_at from the Launch entry
    state$started_at <- incomplete$started_at
    
    # Calculate elapsed time since session started and adjust timer
    if (!is.null(incomplete$started_at) && !is.na(incomplete$started_at)) {
      # Parse the timestamp if it's a string
      started_time <- tryCatch({
        if (is.character(incomplete$started_at)) {
          as.POSIXct(incomplete$started_at, tz = TIMEZONE)
        } else {
          as.POSIXct(incomplete$started_at, tz = TIMEZONE)
        }
      }, error = function(e) {
        with_tz(Sys.time(), tzone = TIMEZONE)
      })
      
      elapsed_sec <- as.integer(difftime(Sys.time(), started_time, units = "secs"))
      # Start timer from the adjusted time
      state$routine_timer_started <- Sys.time() - elapsed_sec
    } else {
      state$routine_timer_started <- Sys.time()
    }
    
    state$session_complete <- FALSE
    
    # Update routine selector
    updateSelectInput(session, "routine_id", selected = incomplete$routine_id)
    
    showNotification("Session resumed!", type = "message", duration = 2)
    state$incomplete_session <- NULL  # Clear the resume prompt
  })
  
  # Dismiss resume handler
  observeEvent(input$dismiss_resume, {
    state$incomplete_session <- NULL
    showNotification("Starting new session...", type = "message", duration = 1)
  })

  observeEvent(input$load_routine, {
    req(input$routine_id)
    # Clear any resume state when starting new routine
    state$incomplete_session <- NULL
    state$resumed_session <- FALSE
    
    state$selected_routine <- input$routine_id
    
    # load steps
    steps <- tryCatch({
      read_sheet_safe(TAB_STEPS) %>%
        filter(.data$routine_id == input$routine_id) %>%
        arrange(.data$step_order)
    }, error = function(e) {
      showNotification(paste0("Error loading steps: ", e$message), type = "error")
      return(data.frame())
    })
    state$steps <- steps
    state$entry_id <- paste0(state$user_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    # Store started_at in UTC
    state$started_at <- store_timestamp_utc(Sys.time())
    state$routine_timer_started <- Sys.time()
    state$session_complete <- FALSE
    
    # Write "Launch" row immediately (all timestamps in UTC)
    launch_start_utc <- store_timestamp_utc(Sys.time())
    launch_row <- data.frame(
      entry_id = state$entry_id,
      user_id = state$user_id,
      routine_id = state$selected_routine,
      item_name = "Launch",
      category = NA,
      started_at = format_utc_for_storage(launch_start_utc),
      ended_at = NA,  # Empty until session completes
      notes = NA,
      cal_event_id = NA,
      created_at = format_utc_for_storage(launch_start_utc),
      stringsAsFactors = FALSE
    )
    tryCatch({
      write_append_entries(launch_row)
    }, error = function(e) {
      showNotification(paste0("Warning: Could not write Launch row: ", e$message), type = "warning")
    })
  })

  output$routine_title <- renderText({
    req(state$routines, state$selected_routine)
    row <- state$routines %>% filter(.data$routine_id == state$selected_routine) %>% slice(1)
    paste0(row$routine_name, " - Manual Mode")  # avoid Unicode em dash to prevent <U+2014>
  })

  # Routine-level elapsed timer (unpausable, stops when session_complete)
  routine_timer <- reactiveTimer(1000)
  output$routine_elapsed <- renderText({
    req(state$routine_timer_started, !state$session_complete)
    routine_timer()
    secs <- as.integer(difftime(Sys.time(), state$routine_timer_started, units = "secs"))
    sprintf(" %02d:%02d", floor(secs/60), secs %% 60)
  })

  observeEvent(input$complete_session, {
    req(state$selected_routine, state$user_id, !state$session_complete)
    state$session_complete <- TRUE  # Stop timer immediately
    # Store ended_at in UTC
    ended_at_utc <- store_timestamp_utc(Sys.time())
    
    # Close any open steps (those still ON)
    if (!is.null(state$steps) && nrow(state$steps) > 0) {
      for (ord in state$steps$step_order) {
        k <- as.character(ord)
        if (!is.null(state$step_start_times[[k]])) {
          # This step is still ON, close it
          step_row <- state$steps %>% filter(.data$step_order == as.integer(k)) %>% slice(1)
          if (nrow(step_row) > 0) {
            tryCatch({
              update_entry_ended_at(state$entry_id, step_row$item_name, ended_at_utc, started_at_hint = state$step_start_times[[k]])
              state$step_frozen_elapsed[[k]] <- as.numeric(difftime(ended_at_utc, state$step_start_times[[k]], units = "secs"))
            }, error = function(e) {
              message("Failed to close step ", step_row$item_name, " on Complete: ", e$message)
            })
          }
        }
      }
    }
    
    # Calculate total elapsed time
    total_sec <- tryCatch({
      as.integer(difftime(Sys.time(), state$routine_timer_started, units = "secs"))
    }, error = function(e) 0)
    if (is.na(total_sec) || total_sec < 0) total_sec <- 0
    total_min <- round(total_sec / 60, 2)

    # Create Google Calendar event if calendar ID is configured (user-specific only, no default fallback)
    cal_event_id <- NA
    message("=== CALENDAR ROUTING DEBUG ===")
    message("Calendar: Current state$user_id='", state$user_id, "'")
    message("Calendar: typeof(state$user_id)=", typeof(state$user_id))
    message("Calendar: is.null(state$user_id)=", is.null(state$user_id))
    message("Calendar: nzchar(state$user_id)=", nzchar(state$user_id))
    cal_id <- resolve_user_calendar_id(state$user_id)
    message("Calendar: resolve_user_calendar_id returned: '", cal_id, "'")
    message("Calendar: typeof(cal_id)=", typeof(cal_id))
    message("Calendar: is.null(cal_id)=", is.null(cal_id))
    message("Calendar: nzchar(cal_id)=", nzchar(cal_id))
    message("=== END CALENDAR DEBUG ===")
    if (!is.null(cal_id) && nzchar(cal_id)) {
      routine_name <- state$routines %>% filter(.data$routine_id == state$selected_routine) %>% pull(.data$routine_name) %>% dplyr::first()

      # Build a concise description from step entries for this entry_id (robust)
      desc <- tryCatch({
        entries <- read_sheet_safe(TAB_ENTRIES)
        req_cols <- c("entry_id", "item_name", "started_at", "ended_at")
        if (!all(req_cols %in% names(entries))) return("")
        e <- entries[entries$entry_id == state$entry_id & !(entries$item_name %in% c("Launch", "Complete and Save")), , drop = FALSE]
        if (nrow(e) == 0) return("")
        # Parse timestamps as UTC (they're stored in UTC now)
        s <- safe_parse_time(e$started_at, tz = "UTC")
        en_raw <- safe_parse_time(e$ended_at, tz = "UTC")
        en <- en_raw
        # If ended_at is missing, use the session end time (already UTC)
        en[is.na(en)] <- ended_at_utc
        ord <- order(s, na.last = TRUE)
        s <- s[ord]; en <- en[ord]; names_ord <- e$item_name[ord]
        # Drop rows without a valid start time
        keep <- !is.na(s)
        s <- s[keep]; en <- en[keep]; names_ord <- names_ord[keep]
        dur_sec <- ifelse(!is.na(s) & !is.na(en), as.integer(difftime(en, s, units = "secs")), NA_integer_)
        fmt <- function(x) ifelse(is.na(x) | x < 0, "—", sprintf("%02d:%02d", floor(x/60), x %% 60))
        lines <- paste0(seq_along(names_ord), ". ", names_ord, ": ", fmt(dur_sec))
        paste0(
          "User: ", state$user_id, "\n",
          "Routine: ", routine_name %||% "Practice", "\n",
          "Total: ", total_min, " min\n",
          "Steps (mm:ss):\n",
          paste(lines, collapse = "\n")
        )
      }, error = function(e) { message("Calendar desc error: ", e$message); "" })
      if (nzchar(desc)) message("Calendar payload description (first 200 chars): ", substr(desc, 1, 200))

      # Calendar events: use user's timezone for display, but compute from UTC timestamps
      started_at_display <- display_timestamp_local(state$started_at, state$user_id)
      ended_at_display <- display_timestamp_local(ended_at_utc, state$user_id)
      res <- tryCatch({
        create_calendar_event(
          state$entry_id,
          routine_name %||% "Practice",
          started_at_display,
          ended_at_display,
          cal_id,
          description = if (nzchar(desc)) desc else paste0("User: ", state$user_id, "\nTotal: ", total_min, " min")
        )
      }, error = function(e) list(id = NULL, status = NA_integer_, body = NULL, error = e$message))
      if (!is.null(res$id)) {
        cal_event_id <- res$id
      } else {
        showNotification(paste0("Calendar failed (", res$error %||% "unknown", ")"), type = "warning", duration = 5)
      }
    } else {
      message("No calendar ID found for user ", state$user_id, "; skipping calendar event")
    }
    
    # Write "Complete and Save" row (all timestamps in UTC)
    complete_end_utc <- ended_at_utc
    complete_row <- data.frame(
      entry_id = state$entry_id,
      user_id = state$user_id,
      routine_id = state$selected_routine,
      item_name = "Complete and Save",
      category = NA,
      started_at = format_utc_for_storage(complete_end_utc),
      ended_at = format_utc_for_storage(complete_end_utc),
      notes = NA,
      cal_event_id = ifelse(is.null(cal_event_id), NA, cal_event_id),
      created_at = format_utc_for_storage(complete_end_utc),
      stringsAsFactors = FALSE
    )
    tryCatch({
      write_append_entries(complete_row)
      cal_msg <- if (!is.null(cal_event_id)) paste0(" (Calendar event created)") else ""
      showNotification(paste0("Practice saved successfully!", cal_msg), type = "message", duration = 3)
      output$save_status <- renderUI(tags$span(style = "color: #090; font-weight:bold;", paste0("✓ Saved (", total_min, " min).", cal_msg)))
      
      # Reset to blank state after 2 seconds
      later(function() {
        state$steps <- NULL
        state$selected_routine <- NULL
        state$routine_timer_started <- NULL
        state$entry_id <- NULL
        state$started_at <- NULL
        state$session_complete <- FALSE
        updateSelectInput(session, "routine_id", selected = "")
        output$save_status <- renderUI(NULL)
      }, delay = 2)
    }, error = function(e) {
      showNotification(paste0("Save failed: ", e$message), type = "error", duration = 5)
      output$save_status <- renderUI(tags$span(style = "color: #c00;", paste0("✗ Save failed: ", e$message)))
      state$session_complete <- FALSE  # Re-enable on error
    })
  })

  output$steps_ui <- renderUI({
    req(state$steps)
    steps <- state$steps
    tags$div(
      class = "steps-container",
      lapply(seq_len(nrow(steps)), function(i) {
        ord <- as.character(steps$step_order[i])
        nm <- steps$item_name[i]
        
        # Check for display_text (flexible column name)
        display_txt <- NULL
        if ("display_text" %in% names(steps)) {
          val <- steps$display_text[i]
          if (!is.na(val) && nzchar(as.character(val))) {
            display_txt <- as.character(val)
          }
        }
        
        # Check for intended_duration or intended_duration_min
        intended_dur <- NULL
        dur_col <- NULL
        if ("intended_duration" %in% names(steps)) {
          dur_col <- steps$intended_duration
        } else if ("intended_duration_min" %in% names(steps)) {
          dur_col <- steps$intended_duration_min
        }
        if (!is.null(dur_col) && !is.na(dur_col[i])) {
          intended_dur <- as.numeric(dur_col[i])
          if (is.na(intended_dur)) intended_dur <- NULL
        }
        
        tags$div(
          fluidRow(
            class = "step-row",
            column(5, 
              tags$div(
                tags$b(nm, class = "step-name"),
                if (!is.null(display_txt)) tags$div(display_txt, class = "step-display-text")
              )
            ),
            column(2, style = "text-align: center; padding: 0 5px;",
              if (!is.null(intended_dur)) tags$span(paste0(intended_dur, " min"), class = "step-intended-duration") else tags$span("")
            ),
            column(2, style = "text-align: right; padding: 0 5px;", tags$span(textOutput(paste0("timer_", ord)), class = "step-timer")),
            column(3, div(class = "step-toggle",
              switchInput(
                inputId = paste0("toggle_", ord),
                label = NULL,
                value = FALSE,
                onLabel = "On",
                offLabel = "Off",
                size = "large",
                width = "110px"
              )
            ))
          )
        )
      })
    )
  })

  output$routine_description <- renderText({
    req(state$selected_routine)
    routine <- state$routines %>% filter(.data$routine_id == state$selected_routine) %>% slice(1)
    if (nrow(routine) > 0) {
      desc <- routine$description
      if (!is.null(desc) && !is.na(desc) && nzchar(as.character(desc))) {
        as.character(desc)
      } else {
        ""
      }
    } else {
      ""
    }
  })

  # Per-step toggle handlers + timers
  observeEvent(state$steps, {
    req(state$steps, nrow(state$steps) > 0)
    steps <- isolate(state$steps)
    for (ord in steps$step_order) {
      local({
        k <- as.character(ord)
        # Toggle handler
        observeEvent(input[[paste0("toggle_", k)]], {
          toggle_value <- input[[paste0("toggle_", k)]]
          step_row <- isolate(state$steps) %>% filter(.data$step_order == as.integer(k)) %>% slice(1)
          if (nrow(step_row) == 0) return()
          eid <- isolate(state$entry_id)

          if (toggle_value) {
            # Start: remember start time in UTC, clear frozen
            current_time_utc <- store_timestamp_utc(Sys.time())
            state$step_start_times[[k]] <- current_time_utc
            state$step_frozen_elapsed[[k]] <- NULL

            # Append start row (all timestamps in UTC)
            entry_row <- data.frame(
              entry_id = eid,
              user_id = isolate(state$user_id),
              routine_id = isolate(state$selected_routine),
              item_name = step_row$item_name,
              category = step_row$category %||% NA,
              started_at = format_utc_for_storage(current_time_utc),
              ended_at = NA,
              notes = NA,
              cal_event_id = NA,
              created_at = format_utc_for_storage(current_time_utc),
              stringsAsFactors = FALSE
            )
            tryCatch({
              write_append_entries(entry_row)
              showNotification(paste0("Started: ", step_row$item_name), type = "message", duration = 1)
            }, error = function(e) {
              showNotification(paste0("Failed to start ", step_row$item_name, ": ", e$message), type = "error", duration = 3)
            })
          } else {
            # Stop: freeze elapsed, update ended_at in sheet (store in UTC)
            current_time_utc <- store_timestamp_utc(Sys.time())
            if (!is.null(state$step_start_times[[k]])) {
              state$step_frozen_elapsed[[k]] <- as.numeric(difftime(current_time_utc, state$step_start_times[[k]], units = "secs"))
            }
            tryCatch({
              success <- update_entry_ended_at(eid, step_row$item_name, current_time_utc, started_at_hint = state$step_start_times[[k]])
              if (success) {
                showNotification(paste0("Stopped: ", step_row$item_name), type = "message", duration = 1)
              }
              # Don't show "No active session found" - this is expected when toggling OFF an already-closed step
            }, error = function(e) {
              showNotification(paste0("Failed to stop ", step_row$item_name, ": ", e$message), type = "error", duration = 3)
            })
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        # Timer renderer
        output[[paste0("timer_", k)]] <- renderText({
          invalidateLater(1000, session)
          start_time <- state$step_start_times[[k]]
          frozen <- state$step_frozen_elapsed[[k]]
          is_on <- isTRUE(input[[paste0("toggle_", k)]])
          secs <- 0
          if (is_on && !is.null(start_time)) {
            secs <- as.integer(difftime(Sys.time(), start_time, units = "secs"))
          } else if (!is_on && !is.null(frozen)) {
            secs <- as.integer(frozen)
          }
          sprintf("%02d:%02d", floor(secs/60), secs %% 60)
        })
      })
    }
  }, ignoreInit = FALSE, once = TRUE)
}

# Return the Shiny app object (works in RStudio and Rscript)
shinyApp(ui, server)

