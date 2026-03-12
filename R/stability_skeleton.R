source('R/TROLL_read_data.R')
source('R/TROLL_rename_cols.R')
source('R/is_stationary.R')


# 0. Required Inputs ----
path = 'inst/extdata/2025-09-16_LT1.csv'
min_median_seconds = 10
stationary_depth_threshold = 0.1


# 1. Read Data ----
dat_read <- TROLL_read_data(path = path)
# 2. Rename Column and Clean unnecessary data ----
dat_rename <- TROLL_rename_cols(df = dat_read,
                                trollcomm_serials = trollCOMM_serials,
                                strip_metadata = TRUE,
                                print_colnames = FALSE)
# 3. Detect when sonde is stationary ----
dat_stationary <- is_stationary(df = dat_rename,
                                depth_col = depth_m,
                                datetime_col = DateTime,
                                depth_range_threshold = 0.1,
                                stationary_secs = 45,
                                rolling_range_secs = 10,
                                start_trim_secs = 4,
                                drop_cols = TRUE,
                                plot = TRUE)
# 4. Optionally match stationary depths to target depths ----
# troll_run_stats()
# 5. Iteratively check sensor stability
dat_stable <- troll_sensor_stable(dat_stationary)
