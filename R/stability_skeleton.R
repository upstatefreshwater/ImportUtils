# 0. Required Inputs ----
path = ''
min_median_seconds = 10
stationary_depth_threshold = 0.1


# 1. Read Data ----
  read_datafile()
# 2. Rename Column and Clean unnecessary data ----
  rename_trollcols()
# 3. Detect when sonde is stationary ----
  is_stationary()
# 4. Optionally match stationary depths to target depths ----
  troll_run_stats()
# 5. Iteratively check sensor stability
  troll_sensor_stable()
