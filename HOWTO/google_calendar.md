# HOWTO: Connect Google Calendar (service account)

## Goal
Create calendar events when a module completes, using a Google service account.

## Steps
1. **Service account**: Use the same JSON key used for Google Sheets (`GS_SERVICE_JSON`).
2. **Scopes**: App authenticates with both Sheets + Calendar scopes.
3. **Calendar ID**: Decide the target calendar per user. For `Manual_Practice_Beta`, set:
   - In `users` sheet: `google_calendar_id` per user, or
   - In `config.R`: `DEFAULT_CALENDAR_ID` as a fallback
4. **Share calendar**: In Google Calendar, share the target calendar with the service account `client_email` and grant “Make changes to events”.
5. **Enable in app**: On session completion, the app calls a helper to create the event using `googleCalendarR::gc_event`.

## Env/config
- `MPB_GS_SERVICE_JSON` — absolute path to your JSON key (server + local)
- `DEFAULT_CALENDAR_ID` — optional; used if user-level calendar not provided

## Troubleshooting
- 403/insufficient permissions: the calendar isn’t shared with the service account or scope missing
- Event not appearing: confirm correct calendar ID and timezone
