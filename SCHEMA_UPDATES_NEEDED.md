# Schema Updates Needed in Google Sheets

## routine_steps tab
- ✅ Change column `item_order` to `step_order` (already done in your sheet)

## practice_entries tab
Current columns (keep all):
- `entry_id` (leave blank/auto - we use entry_group_id for grouping)
 Kalendar- `entry_group_id` (groups all rows for one submission)
- `user_id`
- `routine_id`
- `item_name` (will contain: "Launch", step names, "Complete and Save")
- `category` (nullable)
- `started_at` (timestamp)
- `ended_at` (timestamp)
- `duration_min` (numeric)
- `notes` (nullable)
- `cal_event_id` (nullable, for future calendar integration)
- `created_at` (timestamp - when row was written to sheet)

**No new columns needed** - current schema supports:
- Launch row: `item_name="Launch"`, `started_at=created_at`, `ended_at=started_at`
- Step rows: `item_name=step_name`, proper `started_at`/`ended_at` timestamps
- Complete row: `item_name="Complete and Save"`, `udi_at=ended_at`, `created_at=ended_at`

## Resume Logic (Future)
To check if user has incomplete session:
- Query `practice_entries` for latest row where `user_id = X`
- If latest `item_name != "Complete and Save"`, user has incomplete session
- Can resume from that `entry_group_id`


