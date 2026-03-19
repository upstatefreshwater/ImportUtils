path <-
  'inst/extdata/2025-05-13_LW1.csv'
# 'inst/extdata/2025-05-27_LT1.csv'
# 'inst/extdata/2025-09-16_LT1.csv'
# 'inst/extdata/2025-10-01_LW1.csv'

dat_read <- TROLL_read_data(path)
dplyr::glimpse(dat_read)

dat_rename <- TROLL_rename_cols(df = dat_read,
                                verbose = FALSE)
dplyr::glimpse(dat_rename)

dat_stationary <- is_stationary(df = dat_rename,
                                drop_cols = TRUE)
dplyr::glimpse(dat_stationary)

dat_stable <- TROLL_sensor_stable(df = dat_stationary,
                                  value_col = pH_units)
dplyr::glimpse(dat_stable)

dat_summary <- TROLL_stable_summary(df = dat_stable)
dplyr::glimpse(dat_summary)
dat_summary

TROLL_profile_compiler(path = path,
                       depth_col = depth_m,
                       datetime_col = DateTime
                       )
