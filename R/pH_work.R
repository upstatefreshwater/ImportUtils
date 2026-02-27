# pH_stable ----
pH_stable <- function(df,
                      sd_thresh = 0.01,
                      sampling_int,
                      pH_col = pH_units,
                      datetime_col = DateTime,
                      window_secs = 10){
  if(!('stationary_block_id' %in% names(df))){
    stop('"stationary_block_id" not found in "df". Run "is_stationary()" first.')
  }

  n_range_window <- ceiling(window_secs / sampling_int)              # this is the "width" argument of rollapply in units of "rows"/"observations"

  # ph_dat <- df %>%
  #   group_by(stationary_block_id) |>
  #   dplyr::mutate(
  #     # Calculate DO range across the window ----
  #     ph_sd = zoo::rollapplyr(ifelse(post_jiggle, {{ pH_col }}, NA),                                # 'right' means from the obs. look backwards n = window
  #                                width = n_range_window,
  #                                FUN = stats::sd,                                # Calculate the rolling SD of the window
  #                                na.rm = TRUE,
  #                                fill = NA),
  #    ph_stable = ifelse(
  #       ph_sd <= sd_thresh,
  #       TRUE,
  #       FALSE
  #     )
  #   ) |>
  #   dplyr::ungroup()
  # numeric time in seconds
  time_num <- as.numeric(df |> dplyr::pull({{ datetime_col }}))

  # rolling slope function
  roll_slope <- function(x, t) {
    if (any(is.na(x)) || any(is.na(t))) return(NA_real_)
    coef(stats::lm(x ~ t))[2]
  }

  ph_dat <- df %>%
    dplyr::mutate(
      .time_num = as.numeric({{ datetime_col }})
    ) |>                       # <--- pipe here
    group_by(stationary_block_id) |>
    dplyr::mutate(
      .phcol = {{ pH_col }},

      roll_slope = zoo::rollapplyr(
        seq_along(.phcol),
        width = n_obs_post_jig,
        FUN = function(idx) {
          coef(stats::lm(.phcol[idx] ~ .time_num[idx]))[2]
        },
        fill = NA
      )
    ) |>
    dplyr::ungroup()

  return(ph_dat)
}

# dat3 %>%
#   group_by(obs_depth) %>%
#   filter(DO_withinthresh) %>%
#   summarise(median = median(pH_units,na.rm = T),
#             sd = sd(pH_units,na.rm = T)) %>% dput
xx <-
  dat3 %>% pH_stable(sampling_int = 2)
# View(xx)

ggplot(xx,
       aes(x = DateTime, y = ph_sd, group = obs_depth)) +
  geom_point(aes(color = as.factor(ph_stable))) +
  geom_smooth()

range(xx$ph_sd_per_sec,na.rm = T)



#### ----
stationary_thresh = 998

xx <-
  dat3 %>% ungroup() %>%
  select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,post_jiggle,pH_units) %>%
  filter(post_jiggle & is_stationary_status > stationary_thresh & !is.na(pH_units)) %>%
  group_by(stationary_block_id) %>%
  mutate(t = as.numeric(difftime(DateTime, DateTime[1], units = "mins"))) %>%  # scale time and make it into seconds
  group_split()


junk <- xx[[7]]

min_n = 5
sampling_int = 2 # seconds
slope_thresh = 0.01
df <- junk

out <- tibble(
  stationary_block_id = df$stationary_block_id,
  DateTime = df$DateTime,
  n_used = NA_real_,
  slope = NA_real_,
  slope_per_second = NA_real_,
  n_dropped = NA_real_,
  i = 1:nrow(df),
  meets_thresh = FALSE)

i = 1
dropped <- 0

while (nrow(df)>=min_n) {
  fit <- lm(pH_units ~ t, data = df)                            # fit linear regression across entire data
  slope_fit <<- coef(fit)[["t"]]                                # extract the slope
  # convert to rate (slope/second)
  slope_per_sec <- slope_fit / 60          # convert the slope into a rate

  out$slope[i] <- slope_fit
  out$slope_per_second[i] = slope_per_sec
  out$n_dropped[i] <- dropped
  out$n_used[i] <- nrow(df)

  if(abs(slope_fit) <= slope_thresh){
    out$meets_thresh <- TRUE
  }

  df <- df[-1,]
  dropped <- dropped + 1
  i = i + 1
}

out <- out %>% na.omit()

dat3 %>% ungroup() %>%
  left_join(out,
            by = 'DateTime') %>% view



# try this
fit_until_slope_ok <- function(df,
                               slope_thresh = 0.001,
                               min_n = 3) {

  dropped <- 0
  block_id <- df$stationary_block_id[1]

  while (nrow(df) >= min_n) {

    fit <- lm(pH_units ~ t, data = df)
    slope <<- coef(fit)[["t"]]

    if (abs(slope) <= slope_thresh) {
      return(tibble(
        stationary_block_id = block_id,
        slope = slope,
        n_used = nrow(df),
        n_dropped = dropped
      ))
    }

    df <- df[-1, ]
    dropped <- dropped + 1
  }

  tibble(
    stationary_block_id = block_id,
    slope = NA_real_,
    n_used = nrow(df),
    n_dropped = dropped
  )
}

results <-
  dat3 %>%
  ungroup() %>%
  select(DateTime, depth_m, obs_depth,
         stationary_block_id, is_stationary_status,
         post_jiggle, pH_units) %>%
  filter(post_jiggle,
         is_stationary_status > stationary_thresh,
         !is.na(pH_units)) %>%
  group_by(stationary_block_id) %>%
  mutate(t = as.numeric(difftime(DateTime, DateTime[1], units = "mins"))) %>%
  group_modify(~ fit_until_slope_ok(.x, slope_thresh = 0.0001)) %>%
  ungroup()

xx[[1]]
xx[[1]][[2]]

names(xx[[22]])
str(xx)
####
library(dplyr)
library(purrr)

library(dplyr)
library(purrr)

trim_slope_robust <- function(df, threshold = 0.0005) {
  # 1. Clean data: Remove rows where pH is NA immediately
  df <- df %>% filter(!is.na(pH_units))

  # 2. Keep looping as long as we have enough data (min 3 points for stability)
  while (nrow(df) >= 3) {

    # Calculate slope
    fit <- lm(pH_units ~ DateTime, data = df)
    s_coeff <- coef(fit)

    # Check if the slope exists (is not NA)
    if (length(s_coeff) < 2 || is.na(s_coeff[2])) {
      return(df[0, ]) # Return empty if model fails
    }

    current_slope <- abs(s_coeff[2])

    # 3. Logic check: If slope is still too high, trim the left side
    if (current_slope > threshold) {
      df <- df[-1, ]
    } else {
      # Threshold met! Add the final slope as a reference column
      df$final_slope <- current_slope
      return(df)
    }
  }

  # If we exhaust the data without meeting threshold
  return(df[0, ])
}

# Apply to your groups
final_data <- dat3 %>%
  group_by(stationary_block_id) %>%
  group_split() %>%
  map_dfr(~ trim_slope_robust(.x, threshold = 0.0005))

# View results
print(final_data)
