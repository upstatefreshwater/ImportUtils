# Put it together ----
#__________________________________
median_secs <- 30
shake_time <- 15             # aka "jiggle_secs"
target_depths <- seq(0,8,1)
stationary_time_thresh <- 15 # Consecutive seconds without sonde movmement to be considered "stationary"

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
                plot = TRUE)
# try <- stabilize_cast(dat)           # Compiles "samp_int", "cast_len", "num_stationary_depths", and "final_depths"
try <- troll_run_stats(dat)

if(!all(target_depths %in% try$final_depths)) {
  stop('The final depths extracted from raw data (rounded to the interval between target depths) do not match the target depths.\n\nCheck the specification of target depths and the tolerance used to round raw depth data.\n\nOften this is caused by too strict of a depth tolerance relative to imperfect field data.')
}

dat2 <- dat |>
  remove_jiggle(sampling_int = try$samp_int,
                jiggle_secs = shake_time)

# Check for number of obs to calculate median
check_dat <- dat2 |>
  dplyr::filter(post_jiggle==TRUE) |>
  dplyr::group_by(obs_depth) |>
  dplyr::summarise(n_obs_median = unique(n_obs_post_jig)) |>
  dplyr::mutate(times_avail = n_obs_median * try$samp_int,
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
    "\n\nDid not meet the minimum stationary duration criteria of ",median_secs," seconds."
  )

  warning(msg, call. = FALSE)
 }
check_dat



data_final <- data|> group_by(depthwholem)|>arrange(id)|>
  mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
          roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
          slope = abs(c(NA, diff(roll_mean))) )|>
  # Require stability AND low slope
  mutate(stable = roll_sd < 0.20 & slope < 0.10) |>
  # Keep only stable points AFTER first unstable change
  filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) |>
  filter(stable)|> summarise(across( -c(DateTime, depth_halfm, id),
                                      ~ mean(.x, na.rm = TRUE) ), .groups = "drop")|>
  select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
                  "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                  "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                  "latitude_deg", "longitude_deg")))



dat <- read_datafile('inst/extdata/2025-05-27_LT1.csv')
dat <- read_datafile('inst/extdata/2025-09-16_LT1.csv')
# dat <- read_datafile('inst/extdata/2025-05-13_LW1.csv')
# dat <- read_datafile('inst/extdata/2025-10-01_LW1.csv')


dat <- rename_cols(dat)

try <- is_stationary(dat,drop_cols = T,
                     plot = F)


xx <- try |>
  dplyr::select(DateTime,depth_m,temperature_C,DO_mgL,pH_units)
# xx

# rolling slope function
roll_slope <- function(x, t) {
  if (any(is.na(x)) || any(is.na(t))) return(NA_real_) # returns NA if NA exists in data (sensor or time)
  coef(stats::lm(x ~ t))[2] # Extract slope of the linear regression
}

junk <-
xx |>
  dplyr::mutate(
    numdate = as.numeric(DateTime),
    param_sd = zoo::rollapplyr(
      pH_units,
      width = 15,
      FUN = stats::sd,
      na.rm = TRUE,
      fill = NA_real_
    ),
    param_slope = zoo::rollapplyr(
      align = 'right',
      data = cbind(numdate, xx$pH_units),
      width = 15,
      by.column = FALSE,
      FUN = function(mat) {
        # print(mat)
        # t <- mat[,1]
        t <- (mat[,1] - mat[1,1]) / 60  # center data to stabilize numerically in minutes for ease of interpretation
        x <- mat[,2]

        if (any(is.na(x)) || any(is.na(t))) return(NA_real_)

        coef(stats::lm(x ~ t))[2]
      },
      fill = NA_real_
    )
  )

#** Go to AI_broken for a skeleton of the implementation
#* 1) Identify stationary blocks
#* 2) Toss 15s of data (may increase, or make adjustable), the "jiggle zone"
#* 3) calculate slope across "stationary" observations
#*    a) Could check the overall (for drift)
#*    b) Use small window slopes (8-10s) for determining "stable"
#* 4) Take median of the stable datapoints

# library(ggplot2)
ggplot(junk,
       aes(x=DateTime)) +
  geom_point(aes(y=param_slope,color = 'slope')) +
  geom_point(aes(y=scale(pH_units),color = 'temp')) +
  coord_cartesian(ylim = c(-1.5,1.5)) +
  geom_hline(yintercept = 0, lty=2) +
  geom_hline(yintercept = c(-0.02,0.02),
             col = 'firebrick',lty = 2)

test <- cbind(as.numeric(xx$DateTime), xx$temperature_C)
test

# coef(stats::lm(test[1:5,1]~test[1:5,2]))[2]

zoo::rollapplyr(
  data = test,
  width = 5,
  by.column = FALSE,
  FUN = function(mat) {
     # print(mat)
    # t <- mat[,1]
    t <- (mat[,1] - mat[1,1]) / 60  # center data to stabilize numerically in minutes for ease of interpretation
    x <- mat[,2]

    if (any(is.na(x)) || any(is.na(t))) return(NA_real_)

    coef(stats::lm(x ~ t))[2]

  }
)

plotstuff <- function(df,param){
  trash <- df |> dplyr::ungroup() |>
    dplyr::select(DateTime,depth_m,depthwholem,!!sym(param),
                  is_stationary_status) |>
    dplyr::mutate(x = c(depthwholem - lag(depthwholem,default = 0)),
           label = ifelse(x==1,depthwholem,NA))

  ggplot2::ggplot(trash) +
    ggplot2::geom_point(ggplot2::aes(x = DateTime, y = !!sym(param),
                                     color = as.factor(is_stationary_status))) +
    ggplot2::geom_label(ggplot2::aes(x = DateTime, y = !!sym(param),label = label),
               nudge_y = 0.25)
}

plotstuff(df = try,'temperature_C')
plotstuff(df = dat,'DO_mgL')
plotstuff(df = dat, 'depth_m')



# Exploratory plots ----
library(tidyverse)

ggplot(data = try) +
  geom_point(aes(x = DO_mgL,y = depth_m, color = 'Raw')) +
  geom_point(aes(x = temp_median, y = obs_depth, color = 'Median')) +
  scale_y_reverse()

ggplot(data = try) +
  geom_point(aes(x = DateTime, y = temperature_C, color = 'Raw')) + # temperature_C-15 # depth_m
  geom_point(aes(x = DateTime, y = temp_median, color = 'Median')) +
  geom_vline(xintercept = c(ymd_hms('2025/09/16 15:17:55'),
                            ymd_hms('2025/09/16 15:19:23'))) + # Depth jump from 6 to 7 m
  annotate('text', label = '6 -> 7m',
           x = ymd_hms('2025/09/16 15:17:55'),
           y = 20,
           hjust = -0.1,
           angle = 0) +
  annotate('text', label = '7 -> 8m',
           x = ymd_hms('2025/09/16 15:19:23'),
           y = 22,
           hjust = -0.1,
           angle = 0)

  coord_cartesian(ylim = c(0,0.05))

coord_cartesian(xlim = c(ymd_hms('2025/09/16 15:19:23'),
                           ymd_hms('2025/09/16 15:21:23')))
