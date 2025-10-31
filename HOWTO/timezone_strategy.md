# HOWTO: Timezone Strategy

## Core Principle
**Always store timestamps in UTC. Always display timestamps in user's timezone (unless viewing "true time").**

## Storage Format
- **Database/Sheets**: All timestamps stored as UTC (ISO 8601 format preferred: `YYYY-MM-DDTHH:MM:SSZ` or `YYYY-MM-DD HH:MM:SS` with explicit UTC assumption)
- **R Code**: Use `POSIXct` objects with `tz = "UTC"` internally
- **Display**: Convert to user's timezone only at render time

## User Timezone Management
- Each user has a `timezone` column in the `users` sheet (e.g., `America/New_York`, `Europe/Vienna`)
- Users can update their timezone if traveling
- Default timezone can be set in config, but per-user overrides

## Two Display Modes
1. **Subjective Time** (default for user's own data)
   - User sees their timestamps in their local timezone
   - "I did my routine at 7:00 AM" (their local 7 AM)
   - Calendar events show at the time the user experienced them locally

2. **True Time** (for cross-user analysis)
   - All users' data displayed in a single timezone (viewer's or UTC)
   - Useful for seeing simultaneous activities across timezones
   - "Michael did his at 12:00 UTC, Julian did his at 12:30 UTC" (same absolute moment)

## Implementation Pattern

### Writing Timestamps
```r
# When user creates an action at "their local time"
local_time <- Sys.time()  # System time (user's machine timezone)
utc_time <- lubridate::with_tz(local_time, tzone = "UTC")
# Store utc_time to sheet
```

### Reading Timestamps
```r
# Read UTC from sheet
utc_time <- as.POSIXct(sheet_value, tz = "UTC")
# Display in user's timezone
user_tz <- get_user_timezone(user_id)  # e.g., "America/New_York"
display_time <- lubridate::with_tz(utc_time, tzone = user_tz)
format(display_time, "%Y-%m-%d %H:%M:%S")
```

### Calendar Events
- Calendar API uses the user's timezone for event display
- Internally we compute from UTC timestamps
- Event shows at "7:00 AM" in user's calendar (their subjective experience)

## Travel Scenario
- User updates `timezone` in users sheet before/while traveling
- Future timestamps will use new timezone for display
- Past timestamps remain in UTC (unchanged), display in current timezone setting

## Code Helpers
- `get_user_timezone(user_id)` → returns user's timezone string or default
- `store_utc(timestamp)` → converts local to UTC and stores
- `display_local(utc_timestamp, user_id)` → converts UTC to user's local timezone for display

## Migration Notes
- Existing timestamps assumed to be in `TIMEZONE` config value (legacy)
- New writes use UTC
- Display layer converts for backward compatibility

