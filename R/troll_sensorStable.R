
df = dat_stationary
value_col = 'DO_mgL'
sampling_int = 2
# range_window_secs = 10
# range_thresh = 0.1
# slope_thresh = 0.1
min_n = 5
stationary_thresh = 998                                                          # Setting this allows the user to decrease stationary time if necessary to meet min_n
remove_jiggle = TRUE

n_range_window = 5

# 1. Input Checks ----
req_cols <- c('DateTime','obs_depth','stationary_block_id','is_stationary_status',value_col) # 'depth_m',

if(!all(req_cols %in% names(df))){
  missingCols <- req_cols[!req_cols %in% names(df)]
  stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
}

jiggle_check <- 'post_jiggle' %in% names(df)
if(!remove_jiggle && jiggle_check){
  warning('You have elected to inlcude "jiggle period" data for stability determination, even though "post_jiggle" flags exist in the input data.')
}



# 2. Data wrangling ----

# Filter to stationary blocks and remove any NA data values
dat_base <- df |>
  dplyr::filter(
    is_stationary_status > stationary_thresh,
    !is.na(.data[[value_col]])
  )

# If jiggle period is flagged in the "post_jiggle" column, and user wishes to remove those rows of data
if (remove_jiggle && jiggle_check) {
  dat_base <- dat_base |>
    dplyr::filter(post_jiggle)
}

# Group the data into stationary blocks and split them out into a list
dat_grouped <- dat_base |>
  dplyr::group_by(stationary_block_id) |>
  dplyr::group_split()





if(remove_jiggle && jiggle_check){
  # Data if jiggle period is flagged
  dat_grouped <- df |>
    dplyr::filter(post_jiggle,
                  is_stationary_status > stationary_thresh,
                  !is.na(value_col)) |> #{{value_col}}
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      range = max(value_col) - min(value_col),
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")),
      slope = stats::cov(t, value_col) / stats::var(t)
      # roll_range = zoo::rollapplyr(
      #   value_col,
      #   width = n_range_window,
      #   FUN = function(x) if(all(!is.na(x))) max(x) - min(x) else NA,
      #   fill = NA
      # )
    ) |>
    dplyr::group_split()
} else{
  dat_grouped <- df |>
    # dplyr::filter(is_stationary_status > stationary_thresh,
    #               !is.na(value_col)) |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      range = max(value_col) - min(value_col),
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")),
      slope = stats::cov(t, value_col) / stats::var(t)
      # roll_range = zoo::rollapplyr(
      #   value_col,
      #   width = n_range_window,
      #   FUN = function(x) if(all(!is.na(x))) max(x) - min(x) else NA,
      #   fill = NA
      # )
    ) |>
    dplyr::group_split()
}

# 3.
p1 <- ggplot2::ggplot(data = dat_rnddepth |>
                        dplyr::mutate(depthdiff = c(NA,diff(depth_m))),
                      ggplot2::aes(x = DateTime,y=depth_m*-1)) +
  ggplot2::geom_point()

p2 <- ggplot2::ggplot(data = dat_rnddepth |> dplyr::mutate(depthdiff = abs(c(NA,diff(depth_m)))),
                      ggplot2::aes(x = DateTime,y=depthdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = seq(0,1,0.1),col='red',lty=2)
p1/p2


#################################
sensor_stable <- function(df,
                      value_col = DO_mgL,
                      min_n = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                      sampling_int = 2,        # seconds
                      slope_thresh = 0.01,     # units/minute
                      stationary_thresh = 998,
                      remove_jiggle = TRUE){

  # 0. Extract value column data using tidy eval and value column name
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)
  # 1. Input checks ----

  req_cols <- c('DateTime','obs_depth','stationary_block_id','is_stationary_status',value_col) # 'depth_m',

  if(!all(req_cols %in% names(df))){
    missingCols <- req_cols[!req_cols %in% names(df)]
    stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
  }

  jiggle_check <- 'post_jiggle' %in% names(df)
  if(!remove_jiggle && jiggle_check){
    warning('You have elected to inlcude "jiggle period" data for stability determination, even though "post_jiggle" flags exist in the input data.')
  }

  if(!is.numeric(min_n) || min_n < 1) stop("min_n must be numeric >= 1")
  if(!is.numeric(sampling_int) || sampling_int <= 0) stop("sampling_int must be > 0")
  if(!is.numeric(slope_thresh) || slope_thresh < 0) stop("slope_thresh must be >= 0")
  if(!is.numeric(stationary_thresh)) stop("stationary_thresh must be numeric")

  # 2. Data Manipulation ----
  # Filter to stationary blocks and remove any NA data values
  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status > stationary_thresh,
      !is.na(.data[[value_col]])
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
    )
    dplyr::group_split()

    # 3. Compute slope statistics.
  #    Start with entire post-jiggle period, re-calculate dropping an obs. each time until min_n is reached

  # Iterate across each of the stable groups data
  out <- tibble::tibble()

  for (i in seq_along(dat_grouped)) {

    stable_group_dat <- dat_grouped[[i]]                    # Extract one stable group of data

    # intermediate single group data holder
    group_out <- tibble::tibble(
      stationary_block_id = stable_group_dat$stationary_block_id,
      DateTime = stable_group_dat$DateTime,
      n_used = NA_real_,
      slope = NA_real_,
      n_dropped = NA_real_,
      i = 1:nrow(stable_group_dat),
      meets_thresh = FALSE)

    # pre-allocate iterators
    j = 1
    dropped <- 0

    # Calculate slope starting using all data, then take one away until only min_n rows are left
    while (nrow(stable_group_dat)>=min_n) {
      # fit <- lm(pH_units ~ t, data = stable_group_dat)                            # fit linear regression across entire data
      # slope_fit <- coef(fit)[["t"]]                                # extract the slope

      # Compute slope analytically to speed up processing time
      x_var <- stable_group_dat[[value_name]]
      y_var <- stable_group_dat$t

      v <- stats::var(y_var)

      slope_fit <- if (is.na(v) || v == 0) {
        NA_real_
        warning(paste0("Single datapoint used for ",unique(stable_group_dat$obs_depth),", slope could not be calculated"))
      } else {
        stats::cov(y_var, x_var) / v
      }

      # slope_fit <- stats::cov(y_var, x_var) / stats::var(y_var)

      group_out$slope[j] <- slope_fit
      group_out$n_dropped[j] <- dropped
      group_out$n_used[j] <- nrow(stable_group_dat)

      if(abs(slope_fit) <= slope_thresh){
        group_out$meets_thresh[j] <- TRUE
      }

      stable_group_dat <- stable_group_dat[-1,]
      dropped <- dropped + 1
      j = j + 1
    }
    # 4. Determine how many rows to keep ----
    # Drop NA tail rows
    # return(group_out)
    group_out <- group_out[!is.na(group_out$slope), ]
    # return(group_out)

    slopes <- group_out$slope

    never_met_thresh <- if(!any(abs(slopes) < slope_thresh)) TRUE else FALSE     # use absolute values to account for negative slopes and positive threshold value

    # If all the slope signs are pointing the same direction, or the slope_thresh was not met, keep only min_n rows
    if(all(slopes >= 0) | all(slopes <=0) | never_met_thresh){
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
    out <- dplyr::bind_rows(out, group_out)
    # out <- out |> dplyr::left_join(group_out,by = 'DateTime','stationary_block_id')
  }

  final <- df |>
    dplyr::left_join(out, by = c("DateTime","stationary_block_id"))

  return(final)
}





