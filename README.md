# Manual_Practice_Beta

A focused Shiny module for “Manual Practice” sessions: launch a routine, toggle steps ON/OFF, capture timestamps to Google Sheets, resume incomplete sessions, and create a Google Calendar event on completion.

## What’s working now
- Launch a routine and immediately log a "Launch" row
- Toggle steps ON/OFF
  - ON: writes a `started_at` row for that step
  - OFF: updates `ended_at` of the same row with robust matching
- Per-step timers shown live (mm:ss), frozen on OFF
- Session resume prompt for incomplete sessions (optional)
- `entry_id` is the grouping key for the entire session
- Default blank user loads as `Guest`; pass `?user=<id>` to select a user
- Diagnostics removed from UI for a clean experience
- Google Calendar integration via service account
  - On Complete & Save, creates an event using the user's `google_calendar_id` from `users` sheet
  - Calendar routing correctly matches each user to their own calendar (no hardcoding)
  - Event description includes only performed steps in submission order with per‑step durations (mm:ss) and total minutes
- Deployed to shinyapps.io
  - Live: https://l9edvk-michael0a0kriegsman.shinyapps.io/experimental_app/

## Config and credentials
- Configure values in `config.R` or env vars (for shinyapps.io):
  - `MPB_SHEET_ID`, `MPB_DEFAULT_USER_ID`, `MPB_TIMEZONE`, `MPB_DEFAULT_CALENDAR_ID`, `MPB_GS_SERVICE_JSON`
- The service account JSON is required for non-interactive Sheets/Calendar access and must be bundled for deployment. It is gitignored.
- HOWTOs under `HOWTO/`:
  - `google_calendar.md` — calendar setup (who does what, where to find calendar ID, sharing with SA)
  - `github_workflow.md` — committing/pushing, branches, tagging
  - `user_profiles.md` — users sheet expectations

## Local run (quick)
```bash
export MPB_GS_SERVICE_JSON="/absolute/path/to/your-service-account.json"
cd ".../Manual_Practice_Beta"
R -q -e "shiny::runApp('.', host='127.0.0.1', port=3840)"
# Open http://127.0.0.1:3840/?user=Michael.Test
```

## Deploy
- We redeploy to the existing shinyapps.io app `experimental_app` (quota-friendly update).
- Script: `Deploy_ManualPracticeBeta.R` (uses `rsconnect::deployApp`).

## Today's changes (summary)
- **Fixed calendar routing bug**: Calendar events now correctly route to each user's calendar (not hardcoded to Michael.Test)
  - Filter out invalid NA/empty user rows before matching
  - Added comprehensive debugging/logging for calendar resolution
- **Timezone strategy implemented**: All timestamps stored in UTC; displayed/used in user's timezone
  - Added `timezone` column support in `users` sheet (per-user timezone)
  - Helper functions: `store_timestamp_utc()`, `display_timestamp_local()`, `get_user_timezone()`
  - Calendar events show at user's local time (subjective experience)
  - Fixed 4-hour timezone offset by ensuring all timestamps stored as UTC
  - See `HOWTO/timezone_strategy.md` for full strategy documentation
- Stabilized ended_at updates and step matching; removed reliance on full-row rewrites
- Switched session grouping to `entry_id` throughout
- Implemented per-step timers and froze on OFF
- Removed diagnostics block; simplified UI
- Set blank default user to load as `Guest`
- Hardened Sheets reads and timestamp parsing (multiple formats; tolerant of blanks)
- Reworked Calendar integration to use direct API with service account (token via gargle)
  - Added concise event descriptions with only the steps you did, in order, with mm:ss durations
- Added local logging and clearer error messages
- Unified repo setup and pushes via SSH remote

## Next steps (prioritized)
1. **Session resume after disconnect (P0 - Priority)**
   - On app load, detect incomplete sessions from `practice_entries` sheet
   - Display "Resume" button instead of "Launch Routine" when incomplete session found
   - Restore all timers: routine elapsed continues ticking, step timers show frozen time if OFF or continue if ON
   - Reconstruct state from existing rows (entry_id, started_at, step ON/OFF states from timestamps)
   - Architecture supports this via timestamps, but requires careful state reconstruction
   - **Status:** Infrastructure exists (`check_incomplete_session` function), needs UI and state restoration logic
2. Calendar robustness (P0) — **DONE**
   - ✅ Per-user timezone override (from `users` sheet); verified event TZ
   - ✅ Calendar routing to correct user's calendar (fixed filter bug)
   - Optional: add location or custom title suffix from routine metadata
3. Reliability polish (P1)
   - Debounce rapid toggle ON/OFF flickers to avoid duplicate row writes
   - Guard against intermittent Sheets latency; retry range updates on OFF
   - Ensure description always reflects the latest writes (consider 200–400ms wait before read)
4. Data shape + schema hygiene (P1)
   - Audit for any lingering `entry_group_id` references; remove entirely
   - Confirm headers match `schema_headers.md` in all code paths
5. UI improvements (P1)
   - Persist per-step elapsed mm:ss across resume (init from existing row)
   - Add subtle visual feedback on successful OFF write (e.g., checkmark)
   - Keyboard shortcuts for quick toggles (if desired)
6. Performance (P2)
   - Cache static tabs (`routines`, `routine_steps`) per session; invalidate on demand
   - Batch writes if a user toggles multiple steps quickly
7. Observability (P2)
   - Add optional verbose logging toggle in UI (dev only)
   - Structured log lines for writes/updates (row id, step, time)
8. Security and secrets (P2)
   - Centralize secrets handling for shinyapps.io (env vars only; avoid absolute paths)
   - Document rotation steps in HOWTO
9. Testing + QA (P2)
   - Add lightweight test cases for `update_entry_ended_at`, time parsing, and description builder
   - Smoke script to simulate a short session and verify sheet + event creation
10. Integration with the Unify shell (P2)
    - Define a minimal module contract (routes/params, user resolution) and surface this module via the landing app
11. Documentation (P2)
    - Add a `.cursorrules` to streamline collab with AI and humans
    - Expand HOWTOs with common troubleshooting (e.g., calendar 403, sheet permissions)

---
Maintained by Michael. Fast-moving—expect frequent updates. Open issues/PRs welcome.


