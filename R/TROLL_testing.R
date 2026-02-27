# Helpers currently live here:
source('R/DSA_TrollCleaner.R')

# 1) This is the skeleton of the main function ----
median_secs <- 30
shake_time <- 15             # aka "jiggle_secs"
target_depths <- seq(0,8,1)
stationary_time_thresh <- 15 # Consecutive seconds without sonde movmement to be considered "stationary"
rolling_range_secs <- 10     # Window size used to compile rolling ranges
depth_plot = FALSE

target_interval <- unique(na.omit(signif(diff(target_depths))))

if(length(target_interval)!=1){
  warning('Provided target depths are not of a regular interval.')
  target_interval <- NULL
}

dat <- read_datafile('inst/extdata/2025-09-16_LT1.csv') |>
  rename_cols() |>                   # Makes pretty and standardized column names
  strip_meta() |>                    # removes unnessary columns
  depth_rounder(interval = target_interval,
                tolerance = 0.2) |>                 # Adds 'obs_depth' and 'flag_depth' columns
  is_stationary(stationary_secs = stationary_time_thresh,                    # Adds 'is_stationary_status' column
                sampling_int = target_interval,
                drop_cols = F,
                plot = depth_plot)
# try <- stabilize_cast(dat)           # Compiles "samp_int", "cast_len", "num_stationary_depths", and "final_depths"
try <- troll_run_stats(dat)

# Pull out the sampling interval
sampling_interval_calculated <- try$samp_int

if(!all(target_depths %in% try$final_depths)) {
  stop('The final depths extracted from raw data (rounded to the interval between target depths) do not match the target depths.\n\nCheck the specification of target depths and the tolerance used to round raw depth data.\n\nOften this is caused by too strict of a depth tolerance relative to imperfect field data.')
}

dat2 <- dat |>
  remove_jiggle(sampling_int = sampling_interval_calculated,
                jiggle_secs = shake_time)

# Check for number of obs to calculate median
check_dat <- dat2 |>
  dplyr::filter(post_jiggle==TRUE) |>
  dplyr::group_by(obs_depth) |>
  dplyr::summarise(n_obs_median = unique(n_obs_post_jig)) |>
  dplyr::mutate(times_avail = n_obs_median * sampling_interval_calculated,
                flag_nobs_toofew = ifelse(times_avail < median_secs,
                                          times_avail,
                                          FALSE)) |>
  dplyr::ungroup()

if(any(check_dat$flag_nobs_toofew == 0)) {
  bad_depths <- check_dat$obs_depth[check_dat$flag_nobs_toofew!=0]
  bad_times <- check_dat$times_avail[check_dat$flag_nobs_toofew!=0]

  bad_dat <- data.frame(depth = bad_depths,
                        seconds = bad_times)

  msg <- paste0(
    "Stationary blocks detected at the following depths:\n",
    paste0(
      "  • ", bad_depths, " m for ", bad_times, " s",
      collapse = "\n"
    ),
    "\n\nDid not meet the minimum stationary duration criteria of ",median_secs," seconds\nfollowing the 'jiggle' period"
  )

  warning(msg, call. = FALSE)
}

dat3 <- dat2 |>
  troll_rollRange(sampling_int = sampling_interval_calculated)


ph_testdat <- dat3 |>
  pH_stable(sampling_int = sampling_interval_calculated,
            slope_thresh = 0.01) # units/second

# 2. Exploratory plots for other params ----
library(ggplot2)
pdat <- dat3 |>
  dplyr::filter() # only keeps "stable" data after the "jiggle" period

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=pH_units,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index')

ggplot(data = pdat,
       aes(x=pH_units,y=obs_depth*-1)) +
  geom_point() + geom_path()

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=chlorophyll_RFU,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index') +
  coord_cartesian(ylim = c(0,0.05))

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=bga_fluorescence_RFU,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index') +
  coord_cartesian(ylim = c(0,0.05))

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=sp_conductivity_uScm,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index') +
  coord_cartesian(ylim = c(80,90))

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=ORP_mV,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index')

ggplot(data = pdat,
       aes(x=seq_len(nrow(pdat)),
           y=turbidity_NTU,
           color=as.factor(obs_depth))) +
  geom_point() + geom_path() +
  labs(x = 'Observation Index') +
  coord_cartesian(ylim = c(0,2.5))
