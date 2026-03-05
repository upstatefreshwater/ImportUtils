#################################
calc_stats <- function(x,y){

  v <- stats::var(x)

  slope <- if(is.na(v) || v == 0) NA_real_
  else stats::cov(x,y)/v

  list(
    slope = slope,
    range = diff(range(y)),
    sd = stats::sd(y)
  )
}
##############################################################
sensor_stable <- function(df,
                      value_col = pH_units,
                      min_n = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                      # sampling_int = 2,        # seconds
                      slope_thresh = 0.05,     # units/minute
                      stationary_thresh = 998,
                      remove_jiggle = TRUE){

  # 0. Extract value column data and value column name using tidy eval
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)
  # 1. Input checks ----

  req_cols <- c('DateTime','depth_m','obs_depth','stationary_block_id','is_stationary_status',value_name) # 'depth_m',

  if(!all(req_cols %in% names(df))){
    missingCols <- req_cols[!req_cols %in% names(df)]
    stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
  }

  jiggle_check <- 'post_jiggle' %in% names(df)
  if(!remove_jiggle && jiggle_check){
    warning('You have elected to include "jiggle period" data for stability determination, even though "post_jiggle" flags exist in the input data.')
  }

  if(!is.numeric(min_n) || min_n < 1) stop("min_n must be numeric >= 1")
  # if(!is.numeric(sampling_int) || sampling_int <= 0) stop("sampling_int must be > 0")
  if(!is.numeric(slope_thresh) || slope_thresh < 0) stop("slope_thresh must be >= 0")
  if(!is.numeric(stationary_thresh)) stop("stationary_thresh must be numeric")

  # 2. Data Manipulation ----
  # Filter to stationary blocks and remove any NA data values
  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status > stationary_thresh,
      !is.na(.data[[value_name]])
    )

  # If jiggle period is flagged in the "post_jiggle" column, and user wishes to remove those rows of data
  if (remove_jiggle && jiggle_check) {
    dat_base <- dat_base |>
      dplyr::select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,post_jiggle,{{value_col}}) |>
      dplyr::filter(post_jiggle)
  } else{
    dat_base <- dat_base |>
      dplyr::select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,{{value_col}})
  }

  # Group the data into stationary blocks and split them out into a list
  dat_grouped <- dat_base |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins"))
    ) |>
    dplyr::group_split()
  # return(dat_grouped)

    # 3. Compute slope statistics.
  #    Start with entire post-jiggle period, re-calculate dropping an obs. each time until min_n is reached

  # Iterate across each of the stable groups data
  # out <- tibble::tibble()
  out_list <- vector("list", length(dat_grouped))

  for (i in seq_along(dat_grouped)) { # seq_along gives list length as 1:length(list)

    stable_group_dat <- dat_grouped[[i]]                    # Extract one stable group of data

    # intermediate single group data holder
    group_out <- tibble::tibble(
      stationary_block_id = stable_group_dat$stationary_block_id,
      DateTime = stable_group_dat$DateTime,
      n_used = NA_real_,
      slope = NA_real_,
      n_dropped = NA_integer_,
      i = 1:nrow(stable_group_dat),
      meets_thresh = FALSE)

    # Set min_n rows slope and meets_thresh flag to keep data if threshold is never met
    if(nrow(group_out) < min_n){
      msg_depth <- unique(stable_group_dat$obs_depth)
      warning(paste0('The given "min_n" is larger than the number of stable data identified for ',value_name,' at',msg_depth,'.'))
    }
    tailstart <- max(1, nrow(group_out) - min_n + 1)
    # group_out$slope[tailstart:nrow(group_out)] <- 1
    group_out$meets_thresh[tailstart:nrow(group_out)] <- TRUE
    # return(group_out)

    # pre-allocate iterators
    row_idx= 1
    dropped <- 0

    # Calculate slope starting using all data, then take one away until only min_n rows are left
    # Right now calculate for every iteration, once proven, we can drop to <thresholds if we want to speed up processing
    while (nrow(stable_group_dat)>=min_n) {
      # fit <- lm(pH_units ~ t, data = stable_group_dat)                            # fit linear regression across entire data
      # slope_fit <- coef(fit)[["t"]]                                # extract the slope

      # Compute slope analytically to speed up processing time
      x_var <- stable_group_dat$t
      y_var <- stable_group_dat[[value_name]]

      v <- stats::var(x_var) # compute sensor data variance

      # Wrapped in protection if someeone specifies min_n = 1
      slope_fit <- if (is.na(v) || v == 0) {
        NA_real_
        warning(paste0("Single datapoint used for ",unique(stable_group_dat$obs_depth),", slope could not be calculated"))
      } else {
        stats::cov(x_var, y_var) / v
        # coef(lm(y_var~x_var))[2]
      }

      group_out$slope[row_idx] <- slope_fit
      group_out$n_dropped[row_idx] <- dropped
      group_out$n_used[row_idx] <- nrow(stable_group_dat)

      if(abs(slope_fit) <= slope_thresh){ # Keep min_n tail rows (designated as slope 1) no matter what  || abs(slope_fit)==1
        group_out$meets_thresh[row_idx] <- TRUE
      }

      stable_group_dat <- stable_group_dat[-1,]
      dropped <- dropped + 1
      row_idx= row_idx+ 1
    }
    # 4. Determine how many rows to keep ----

    slopes <- group_out$slope
    # Remove tail min_n NA rows
    slopes <- slopes[!is.na(slopes)]

    never_met_thresh <- !any(abs(slopes) < slope_thresh)     # use absolute values to account for negative slopes and positive threshold value

    # If all the slope signs are pointing the same direction, or the slope_thresh was not met, keep only min_n rows
    if(all(slopes >= 0) || all(slopes <= 0) || never_met_thresh){
      group_out <- tail(group_out, min_n)
    }

    # Otherwise, keep all rows after the threshold is met
    if(!never_met_thresh){
      first_below_idx <- which(abs(group_out$slope) < slope_thresh)[1]
      r <- nrow(group_out)
      # min_obs_check <- r - first_below_idx + 1
      min_obs_check <- if(!is.na(first_below_idx)) r - first_below_idx + 1 else 0 # Handle case where threshold was never met

      # If first slope threshold met results in < min_n observations, keep min_n
      if(min_obs_check < min_n){ # If min_n = 1, it allows only the final observation be kept
        group_out <- tail(group_out, min_n)
      } else{                    # If slope signal is flat enough and oscillating, keep all rows after first obs < slope_thresh
        group_out <- group_out[first_below_idx:r,]
      }

    }
    # out <- dplyr::bind_rows(out, group_out)
    # out <- out |> dplyr::left_join(group_out,by = 'DateTime','stationary_block_id')

    out_list[[i]] <- group_out
  }
  # Compile the group_out into a dataframe
  out <- dplyr::bind_rows(out_list)

  # join the out metadata with the input dataframe
  final <- df |>
    dplyr::left_join(out, by = c("DateTime","stationary_block_id"))

  return(final)
}

xx <-
sensor_stable(dat_jiggle);xx


yy <- xx |>
  dplyr::group_by(stationary_block_id) |>
  dplyr::mutate(
    t = as.numeric(difftime(DateTime, DateTime[1], units = "mins"))
  )
dplyr::group_split()
