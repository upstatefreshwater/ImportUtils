# Helpers currently live here:
source('R/DSA_TrollCleaner.R')
source('R/rename_trollcols.R')
source('R/is_stationary.R')

# 1) This is the skeleton of the main function ----
median_secs <- 30
shake_time <- 15             # aka "jiggle_secs"
sd_depthrange_thresh <- 0.1 # This is the threshold for the rolling range SD in meters
target_depths <- seq(0,7,1)
stationary_time_thresh <- 15 # Consecutive seconds without sonde movmement to be considered "stationary"
rolling_range_secs <- 10     # Window size used to compile rolling ranges
depth_plot = FALSE

target_interval <- unique(na.omit(signif(diff(target_depths))))

if(length(target_interval)!=1){
  warning('Provided target depths are not of a regular interval.')
  target_interval <- NULL
}

dat_read <-
  read_datafile('inst/extdata/2025-09-16_LT1.csv')
  # read_datafile('inst/extdata/2025-05-07_QL2.csv')

dat_rename <-   rename_trollcols(dat_read)                   # Makes pretty and standardized column names

dat_rnddepth <- depth_rounder(df = dat_rename,
                              interval = target_interval,
                              tolerance = 0.2)                  # Adds 'obs_depth' and 'flag_depth' columns

dat_stationary <- is_stationary(df=dat_rnddepth,
                                depth_range_threshold = sd_depthrange_thresh,
                                stationary_secs = stationary_time_thresh,                    # Adds 'is_stationary_status' column
                                sampling_int = target_interval,
                                drop_cols = FALSE,
                                # window = 10,
                                plot = TRUE)


try <- troll_run_stats(dat_stationary)

# Pull out the sampling interval
sampling_interval_calculated <- try$samp_int

if(!all(target_depths %in% try$final_depths)) {
  missing_depths <- sort(setdiff(target_depths, try$final_depths), decreasing = FALSE)
  extra_depths   <- sort(setdiff(try$final_depths, target_depths), decreasing = FALSE)

  target_sorted  <- sort(target_depths, decreasing = FALSE)
  final_sorted   <- sort(try$final_depths, decreasing = FALSE)

  if (length(missing_depths) > 0 || length(extra_depths) > 0) {
    stop(
      paste0(
        "Mismatch between expected and extracted depths.\n\n",
        "Target depths:    ", paste(target_sorted, collapse = ", "), "\n",
        "Extracted depths: ", paste(final_sorted, collapse = ", "), "\n\n",
        if (length(missing_depths) > 0)
          paste0("Missing depths:   ", paste(missing_depths, collapse = ", "), "\n") else "",
        if (length(extra_depths) > 0)
          paste0("Unexpected depths: ", paste(extra_depths, collapse = ", "), "\n") else "",
        "\nLikely causes:\n",
        "- Depth rounding tolerance is too strict for field variability.\n",
        "- Target depths are irregular or do not match the sampling interval.\n",
        "- Stationary detection removed too much data (check 'stationary_secs' or rolling window).\n\n",
        "Review depth tolerance, stationary thresholds, and raw depth trace."
      )
    )
  }
}

#############################################
ggplot2::ggplot(data = dat_rename,ggplot2::aes(x=DateTime,y=depth_m)) +
  ggplot2::geom_point() + #ggplot2::geom_path()
  ggplot2::geom_hline(yintercept = seq(0,8,1),lty=2,col='red')

dat_jiggle <- dat_stationary |>
  remove_jiggle(sampling_int = sampling_interval_calculated,
                jiggle_secs = shake_time)

# Check for number of obs to calculate median
check_dat <- dat_jiggle |>
  dplyr::ungroup() |>
  dplyr::filter(post_jiggle==TRUE) |>
  dplyr::group_by(obs_depth) |>
  dplyr::reframe(n_obs_median = unique(n_obs_post_jig)) |>
  dplyr::mutate(times_avail = n_obs_median * sampling_interval_calculated,
                flag_nobs_toofew = ifelse(times_avail < median_secs,
                                          times_avail,
                                          FALSE)) |>
  dplyr::ungroup()

if(any(check_dat$flag_nobs_toofew)) {
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

dat3 <- dat_jiggle |>
  troll_rollRange(sampling_int = sampling_interval_calculated)


ph_testdat <- dat3 |>
  pH_stable(sampling_int = sampling_interval_calculated,
            slope_thresh = 0.01) # units/second

# 2. Exploratory plots for other params ----
library(ggplot2)
pdat <- dat3 |>
  dplyr::filter(is_stationary_status&post_jiggle) # only keeps "stable" data after the "jiggle" period

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
