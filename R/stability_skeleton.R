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
dat_stable <- TROLL_sensor_stable(dat_stationary,
                                  value_col = pH_units,
                                  min_secs = 5,
                                  slope_thresh = 0.05,
                                  range_thresh = 0.02,
                                  stationary_thresh = 998,
                                  drop_cols = FALSE)



###########
ggplot(dat_stable, aes(DateTime, pH_units)) +

  # 1. Plot all data faintly so you see the signal
  geom_point(color = "grey80", size = 1) +

  # 2. Highlight stationary periods
  geom_point(
    data = dplyr::filter(dat_stable, is_stationary_status == 999),
    color = "dodgerblue3",
    size = 1.5
  ) +

  # 3. Highlight unstable pH during stationary periods
  geom_point(
    data = dplyr::filter(dat_stable,
                         is_stationary_status == 999,
                         pH_units_stable == FALSE),
    color = "red",
    size = 2
  )


####################
dat_stable <- dat_stable |>
  dplyr::mutate(
    range_scaled = range * 10 + mean(pH_units, na.rm = TRUE)
  )

ggplot(dat_stable, aes(DateTime)) +

  # pH signal
  geom_line(aes(y = pH_units), color = "grey70") +

  # highlight 999 flags
  geom_point(
    data = dplyr::filter(dat_stable, is_stationary_status == 999),
    aes(y = pH_units),
    color = "dodgerblue3",
    size = 1.5
  ) +

  # scaled range
  geom_line(aes(y = range_scaled), color = "orange") +

  # secondary axis
  scale_y_continuous(
    name = "pH_units",
    sec.axis = sec_axis(~ (. - mean(dat_stable$pH_units, na.rm = TRUE))/10,
                        name = "range")
  )

ggplot(dat_stable, aes(DateTime)) +

  geom_line(aes(y = pH_units), color = "grey70") +

  geom_point(
    data = dplyr::filter(dat_stable, is_stationary_status == 999),
    aes(y = pH_units),
    color = "dodgerblue3",
    size = 1.5
  ) +

  geom_line(
    aes(y = range * 10 + mean(pH_units, na.rm = TRUE)),
    color = "orange"
  ) +
  scale_y_continuous(sec_axis(transform = ~.-10/range))

ggplot(dat_stable, aes(DateTime, pH_units)) +
  geom_line(color = "grey40") +
  facet_wrap(~is_stationary_status, scales = "free_x")
