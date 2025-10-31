# Manual Practice Beta — Google Sheets Headers

Copy these into the respective tabs in your Google Sheet.

## users
```
user_id,display_name,timezone,google_calendar_id,created_at,last_modified
```

example row:
```
Michael.Test,Michael (Test),America/New_York,,2025-10-30T09:00:00Z,2025-10-30T09:00:00Z
```

## routines
```
routine_id,user_id,routine_name,description,flow_mode,time_of_day,intended_total_min,routine_type,status,created_at,last_modified
```

example row:
```
morning_test_001,Michael.Test,Morning Practice (Test),Beta test routine,manual,morning,15,general,active,2025-10-30T09:05:00Z,2025-10-30T09:05:00Z
```

## routine_steps
```
routine_id,item_order,item_name,category,intended_duration_min,display_text,created_at
```

example rows:
```
morning_test_001,1,Breathing,Body,5,5 min mindful breathing,2025-10-30T09:06:00Z
morning_test_001,2,Stretching,Body,5,Light stretches,2025-10-30T09:06:00Z
morning_test_001,3,Gratitude,Mind,5,Three gratitudes,2025-10-30T09:06:00Z
```

## practice_entries
```
entry_group_id,user_id,routine_id,item_name,category,started_at,ended_at,notes,cal_event_id,created_at
```

(Leave empty; the app writes rows here.)

**Notes:**
- `entry_group_id`: Groups all rows from one session/routine launch (e.g., "Michael.Test_20251031_045541")
- `started_at`: Timestamp when step started or "Launch" row created
- `ended_at`: Timestamp when step stopped (empty until toggled off)
- Duration can be computed from `started_at` and `ended_at` timestamps if needed

