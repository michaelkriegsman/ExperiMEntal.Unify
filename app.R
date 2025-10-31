suppressPackageStartupMessages({
  library(shiny)
  library(googlesheets4)
  library(dplyr)
  library(lubridate)
  library(later)
  library(shinyWidgets)
  # library(googleCalendarR)  # not required at load; functions are called via namespace
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

invisible(lapply(c("shiny", "googlesheets4", "dplyr", "lubridate", "httr", "jsonlite", "shinyWidgets", "googleCalendarR"), ensure_pkg))

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

# Create Google Calendar event for completed practice session
create_calendar_event <- function(entry_id, routine_name, started_at, ended_at, calendar_id = DEFAULT_CALENDAR_ID) {
  if (is.null(calendar_id) || !nzchar(calendar_id)) {
    message("No calendar ID configured, skipping calendar event creation")
    return(NULL)
  }
  
  # Ensure package is available
  if (!requireNamespace("googleCalendarR", quietly = TRUE)) {
    warning("googleCalendarR package not available; cannot create calendar event")
    return(NULL)
  }
  
  tryCatch({
    # Authenticate with Calendar API using service account
    if (!nzchar(GS_SERVICE_JSON) || !file.exists(GS_SERVICE_JSON)) {
      warning("Service account JSON not found; cannot create calendar event")
      return(NULL)
    }
    
    # Authenticate for Calendar API (separate from Sheets auth)
    googleCalendarR::gc_auth(path = GS_SERVICE_JSON, scopes = GS_SCOPES)
    
    # Create event - ensure times are in correct format
    event <- googleCalendarR::gc_event(
      summary = paste0(routine_name, " - Practice"),
      start = started_at,
      end = ended_at,
      calendarId = calendar_id
    )
    
    if (is.null(event) || is.null(event$id)) {
      warning("Calendar event created but no event ID returned")
      return(NULL)
    }
    
    return(event$id)
  }, error = function(e) {
    # More detailed error message for debugging
    err_msg <- paste0("Failed to create calendar event: ", e$message, 
                     " (Calendar ID: ", calendar_id, ")")
    warning(err_msg)
    return(NULL)
  })
}

# Resolve calendar id for current user (sheet override > default)
resolve_user_calendar_id <- function(user_id) {
  if (is.null(user_id)) return(DEFAULT_CALENDAR_ID)
  cal <- tryCatch({
    users <- read_sheet_safe(TAB_USERS)
    row <- users %>% dplyr::filter(.data$user_id == user_id) %>% dplyr::slice(1)
    if (nrow(row) == 0) return(DEFAULT_CALENDAR_ID)
    val <- as.character(row$google_calendar_id)
    if (length(val) == 0 || is.na(val) || val == "") DEFAULT_CALENDAR_ID else val
  }, error = function(e) DEFAULT_CALENDAR_ID)
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
      message(sprintf("No rows found matching entry_id='%s' and item_name='%s'", entry_id, item_name))
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
    ended_at_str <- format(ended_at, "%Y-%m-%d %H:%M:%S")

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

ui <- fluidPage(
  titlePanel("Manual Practice Beta"),
  sidebarLayout(
    sidebarPanel(
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
      h4(textOutput("routine_title")),
      div(strong("Routine elapsed:"), textOutput("routine_elapsed", inline = TRUE)),
      uiOutput("steps_ui"),
      tags$hr()
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
      q$user %||% DEFAULT_USER_ID
    }, error = function(e) DEFAULT_USER_ID)
    state$user_id <- u
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
    state$started_at <- with_tz(Sys.time(), tzone = TIMEZONE)
    state$routine_timer_started <- Sys.time()
    state$session_complete <- FALSE
    
    # Write "Launch" row immediately
    launch_row <- data.frame(
      entry_id = state$entry_id,
      user_id = state$user_id,
      routine_id = state$selected_routine,
      item_name = "Launch",
      category = NA,
      started_at = state$started_at,
      ended_at = NA,  # Empty until session completes
      notes = NA,
      cal_event_id = NA,
      created_at = state$started_at,
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
    ended_at <- with_tz(Sys.time(), tzone = TIMEZONE)
    # Calculate total elapsed time
    total_sec <- tryCatch({
      as.integer(difftime(Sys.time(), state$routine_timer_started, units = "secs"))
    }, error = function(e) 0)
    if (is.na(total_sec) || total_sec < 0) total_sec <- 0
    total_min <- round(total_sec / 60, 2)

    # Create Google Calendar event if calendar ID is configured (user override > default)
    cal_event_id <- NA
    cal_id <- resolve_user_calendar_id(state$user_id)
    if (nzchar(cal_id)) {
      routine_name <- state$routines %>% filter(.data$routine_id == state$selected_routine) %>% pull(.data$routine_name) %>% dplyr::first()
      tryCatch({
        cal_event_id <- create_calendar_event(
          state$entry_id,
          routine_name %||% "Practice",
          state$started_at,
          ended_at,
          cal_id
        )
        if (is.null(cal_event_id)) {
          showNotification("Calendar event creation failed (check logs)", type = "warning", duration = 3)
        }
      }, error = function(e) {
        showNotification(paste0("Calendar error: ", e$message), type = "warning", duration = 5)
      })
    } else {
      message("No calendar ID found for user ", state$user_id, "; skipping calendar event")
    }
    
    # Write "Complete and Save" row (steps already saved when toggled)
    complete_row <- data.frame(
      entry_id = state$entry_id,
      user_id = state$user_id,
      routine_id = state$selected_routine,
      item_name = "Complete and Save",
      category = NA,
      started_at = ended_at,
      ended_at = ended_at,
      notes = NA,
      cal_event_id = ifelse(is.null(cal_event_id), NA, cal_event_id),
      created_at = ended_at,
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
      lapply(seq_len(nrow(steps)), function(i) {
        ord <- as.character(steps$step_order[i])
        nm <- steps$item_name[i]
        fluidRow(
          column(6, tags$b(nm)),
          column(3, textOutput(paste0("timer_", ord))),
          column(3,
            switchInput(
              inputId = paste0("toggle_", ord),
              label = NULL,
              value = FALSE,
              onLabel = "On",
              offLabel = "Off",
              size = "small",
              width = "100px"
            )
          ),
          tags$hr()
        )
      })
    )
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
          current_time <- with_tz(Sys.time(), tzone = TIMEZONE)
          eid <- isolate(state$entry_id)

          if (toggle_value) {
            # Start: remember start time, clear frozen
            state$step_start_times[[k]] <- current_time
            state$step_frozen_elapsed[[k]] <- NULL

            # Append start row
            entry_row <- data.frame(
              entry_id = eid,
              user_id = isolate(state$user_id),
              routine_id = isolate(state$selected_routine),
              item_name = step_row$item_name,
              category = step_row$category %||% NA,
              started_at = current_time,
              ended_at = NA,
              notes = NA,
              cal_event_id = NA,
              created_at = current_time,
              stringsAsFactors = FALSE
            )
            tryCatch({
              write_append_entries(entry_row)
              showNotification(paste0("Started: ", step_row$item_name), type = "message", duration = 1)
            }, error = function(e) {
              showNotification(paste0("Failed to start ", step_row$item_name, ": ", e$message), type = "error", duration = 3)
            })
          } else {
            # Stop: freeze elapsed, update ended_at in sheet
            if (!is.null(state$step_start_times[[k]])) {
              state$step_frozen_elapsed[[k]] <- as.numeric(difftime(current_time, state$step_start_times[[k]], units = "secs"))
            }
            tryCatch({
              success <- update_entry_ended_at(eid, step_row$item_name, current_time, started_at_hint = state$step_start_times[[k]])
              if (success) {
                showNotification(paste0("Stopped: ", step_row$item_name), type = "message", duration = 1)
              } else {
                showNotification(paste0("No active session found for ", step_row$item_name), type = "warning", duration = 2)
              }
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

# Run app if executed directly (not sourced)
if (!interactive()) {
  shinyApp(ui, server)
}

