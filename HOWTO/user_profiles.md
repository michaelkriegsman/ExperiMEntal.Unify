# HOWTO: User Profiles (Google Sheet)

The `users` tab defines who can use the module and how integrations map.

## Headers
```
user_id,display_name,timezone,google_calendar_id,created_at,last_modified
```

## Add a user
1. Append a new row with a unique `user_id` (e.g., `Michael.Test`)
2. Set `timezone` (e.g., `America/New_York`)
3. Optional: set `google_calendar_id` if this user writes to a specific calendar
4. Set `created_at` and `last_modified` to ISO timestamps

## Update a user
- Change any field and update `last_modified`

## Notes
- The app can read `?user=<user_id>` from the URL to select the user
- Calendar integration uses `google_calendar_id` when present; otherwise uses the default in `config.R`
