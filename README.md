# Manual Practice Beta App

Minimal standalone app for Friday beta: select a routine, run manual steps with start/stop, save practice entries to Google Sheets, create a Google Calendar event.

## Setup

1. Create a Google Sheet (e.g., Manual_Practice_Beta_v1) with tabs:
   - `users`
   - `routines`
   - `routine_steps`
   - `practice_entries`

2. Add headers (copy/paste) and a starter row for testing:

   users (CSV headers):
   ```
   user_id,display_name,timezone,google_calendar_id,created_at,last_modified
   ```
   example row:
   ```
   Michael.Test,Michael (Test),America/New_York,,2025-10-30T09:00:00Z,2025-10-30T09:00:00Z
   ```

   routines (CSV headers):
   ```
   routine_id,user_id,routine_name,description,flow_mode,time_of_day,intended_total_min,routine_type,status,created_at,last_modified
   ```
   example row:
   ```
   morning_test_001,Michael.Test,Morning Practice (Test),Beta test routine,manual,morning,15,general,active,2025-10-30T09:05:00Z,2025-10-30T09:05:00Z
   ```

   routine_steps (CSV headers):
   ```
   routine_id,item_order,item_name,category,intended_duration_min,display_text,created_at
   ```
   example rows:
   ```
   morning_test_001,1,Breathing,Body,5,5 min mindful breathing,2025-10-30T09:06:00Z
   morning_test_001,2,Stretching,Body,5,Light stretches,2025-10-30T09:06:00Z
   morning_test_001,3,Gratitude,Mind,5,Three gratitudes,2025-10-30T09:06:00Z
   ```

   practice_entries (CSV headers):
   ```
   entry_id,entry_group_id,user_id,routine_id,item_name,category,started_at,ended_at,duration_min,notes,cal_event_id,created_at
   ```
   (leave empty; the app will append rows here)

3. Share your Google Sheet with the service account (Editor): the email inside `ExperiMEntal Backend/ExperiMEntal/august-storm-*.json` (also shown in the app when routines fail to load).

4. Update `config.R` values:
   - `SHEET_ID`
   - `DEFAULT_USER_ID` (pre-filled as Michael.Test)
   - `DEFAULT_CALENDAR_ID`
   - `TIMEZONE`

4. Install R packages (first run will auto-install if missing):
   - googlesheets4, shiny, dplyr, lubridate, httr, jsonlite

## Run

In R:
```r
shiny::runApp('Manual_Practice_Beta')
```

## Current Status

✅ **Working:**
- Routine selection and loading
- Launch routine (writes "Launch" row immediately)
- Routine-level timer (unpausable, stops on Complete & Save)
- Save logic (only saves steps with duration > 0)
- Multi-step storage (each step gets its own row)
- Auto-reset after successful save

⏳ **Next Steps:**
- Per-step Start/Stop timers (currently disabled for stability)
- Google Calendar event creation on completion
- Shiny deployment

## Notes

- Calendar event will use simple summary (routine name + total duration) for Friday.
- Step-level durations are recorded per item row in `practice_entries` grouped by `entry_group_id`.
- Only steps with `duration_min > 0` are saved (if no steps started, only Launch + Complete rows).
- Designed to be extractable into the unified med app later.

## URL User Selection

Open with a URL parameter to set user:
```
http://localhost:3850/?user=Michael.Test
```
The app reads `user_id` from the URL (no manual input field).


