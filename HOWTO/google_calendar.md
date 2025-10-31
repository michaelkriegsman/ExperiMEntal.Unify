# HOWTO: Connect Google Calendar (service account)

This guide turns on calendar events when a session completes.

## Overview
- We use the same Google service account JSON used for Sheets.
- Service account email (from your JSON):
  - `experimental-app-sheets@august-storm-464819-q7.iam.gserviceaccount.com`
- On Complete & Save, the app creates an event from `started_at` to the completion time.
- Calendar selection:
  - If the `users` sheet has `google_calendar_id` for the current `user_id`, we use it.
  - Otherwise we fall back to `DEFAULT_CALENDAR_ID` from `config.R` or app env vars.

## Who does what
- **You (one-time per target calendar):**
  1) Decide the calendar to write to (e.g., your primary calendar or a dedicated one).
  2) Share that calendar with the service account email above, with “Make changes to events”.
  3) Put the calendar ID in the `users` sheet (`google_calendar_id`) for each user (e.g., `Michael.Test`).
- **Me (in code / deployment):**
  - Ensure calendar scopes are requested and `googleCalendarR` is available.
  - Resolve per-user calendar at runtime and attempt event creation.
  - Deploy the updated app.

## Prerequisites
- Service account JSON present locally and bundled on deploy.
- App authorized with both Sheets + Calendar scopes (already configured in `app.R`).
- `googleCalendarR` installed on the server (auto-installed during deploy).

## Where to find the Calendar ID
- In Google Calendar (web):
  1) Settings → target calendar → Integrate calendar → copy “Calendar ID”
  2) Example (yours): `1367efeb720a96598ab0743ab0c0cda62b81480d997b3c22ac35ff357400e207@group.calendar.google.com`
  - Tip: `primary` can be used for a main account calendar

## Share the calendar with the service account
- In Google Calendar → Settings → (calendar) → Share with specific people or groups
- Add: `experimental-app-sheets@august-storm-464819-q7.iam.gserviceaccount.com`
- Permission: “Make changes to events”

## Configure the app
- Preferred (per-user): in the `users` sheet, set `google_calendar_id` for that `user_id`.
- Fallback (global): set `DEFAULT_CALENDAR_ID` in `config.R` or as an env var on shinyapps.io.
- Ensure `MPB_GS_SERVICE_JSON` env var points to the JSON filename inside the app bundle (e.g., `august-storm-464819-q7-c26ff636dc17.json`).

## Test
1) Open the app with `?user=<your_user_id>` (e.g., `?user=Michael.Test`)
2) Launch a routine, then Complete & Save
3) Check the target calendar for a new event titled “<routine name> - Practice”

## Troubleshooting
- 403 / insufficient permissions → calendar not shared with service account or wrong permission
- Event not created but save succeeded → verify `google_calendar_id` or `DEFAULT_CALENDAR_ID`
- Wrong timezone → verify the user’s timezone in the `users` sheet and app `TIMEZONE`
