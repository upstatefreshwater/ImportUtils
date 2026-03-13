source('R/TROLL_read_data.R')
source('R/TROLL_rename_cols.R')
source('R/is_stationary.R')
source('R/vectorized_sensor_stable.R')
source('R/utils.R')


# 0. Required Inputs ----
path = 'inst/extdata/2025-09-16_LT1.csv'
depth_col = depth_m
datetime_col = DateTime
depth_range_threshold = 0.1
stationary_secs = 45
rolling_range_secs = 10
start_trim_secs = 4
drop_cols = TRUE
plot = TRUE


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
                                stationary_secs = 35,
                                rolling_range_secs = 10,
                                start_trim_secs = 15,
                                drop_cols = TRUE,
                                plot = TRUE)
# 4. Optionally match stationary depths to target depths ----
# troll_run_stats()
# 5. Iteratively check sensor stability
dat_stable <- TROLL_sensor_stable(dat_rename,
                                  value_col = pH_units,
                                  min_n = 5,
                                  slope_thresh = 0.05,
                                  range_thresh = 0.02,
                                  stationary_thresh = 998,
                                  remove_jiggle = FALSE)


ggplot(data = dat_stable,
       aes(x = DateTime, y = `pH_units`, color = as.factor(pH_units_stable))) +
  geom_point()


ggplot(data = dat_stable,
       aes(x = DateTime, y = `pH_units`, color = as.factor(is_stationary_status))) +
  geom_point()
