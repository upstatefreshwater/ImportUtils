plot_stability <- function(df,
                           value_col_sym,
                           value_flag_col,
                           range_thresh){

  # value_col <- rlang::ensym(value_col)
  # value_name <- rlang::as_name(value_col)
  # value_flag_col <- paste0(value_name,"_stable")
  value_flag_sym <- rlang::sym(value_flag_col)

  rangelines <- median(df[[rlang::as_name(value_col_sym)]],na.rm = TRUE)
  p1 <-
  ggplot2::ggplot(df, ggplot2::aes(DateTime, !!value_col_sym)) +

    # all data as background
    ggplot2::geom_point(ggplot2::aes(color = "Sonde Moving"), size = 1) +

    # stationary periods
    ggplot2::geom_point(
      data = dplyr::filter(df, is_stationary_status == 999),
      ggplot2::aes(color = "Stable Stationary"),
      size = 1.5,
      pch = 19
    ) +

    # unstable pH during stationary periods
    ggplot2::geom_point(
      data = dplyr::filter(df,
                           is_stationary_status == 999,
                           !!value_flag_sym %in% FALSE),
      ggplot2::aes(color = "Unstable Stationary"),
      size = 1.5,
      pch = 19
    ) +

    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "Sonde Moving" = "grey80",
        "Stable Stationary" = "dodgerblue3",
        "Unstable Stationary" = "firebrick1"
      )
    ) +

    # Add a lines representing the range threshold
    ggplot2::geom_hline(yintercept = c(rangelines,rangelines + range_thresh)) +

    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right")

  print(p1)
}
# plot_stability(df = dat_stable,
#                value_col_sym = rlang::ensym(pH_units),
#                value_flag_col = paste0(rlang::as_name(rlang::ensym(pH_units)),"_stable"))

################3
#' Identify Stable Sensor Values Within Stationary Profiling Blocks
#'
#' Evaluates whether sensor measurements collected during stationary profiling
#' periods are stable based on slope and range thresholds. Stability is assessed
#' within each stationary block identified by \code{is_stationary()}, and the
#' function flags observations that meet both slope and range criteria.
#'
#' For each stationary block, the function iteratively evaluates windows of data
#' beginning at each observation and extending to the end of the block. The slope
#' (units per minute) and value range are calculated for each window. If both
#' metrics fall within the specified thresholds, the observation and all
#' subsequent rows in that block are flagged as stable.
#'
#' A minimum duration of data can be enforced using \code{min_secs}, which is
#' converted internally to a minimum number of observations based on the sampling
#' interval.
#'
#' This function requires that stationary blocks have already been identified
#' using \code{\link{is_stationary}}.
#'
#' @param df A data frame containing sensor data and stationary block metadata.
#'
#' @param value_col Unquoted name of the sensor column to evaluate for stability
#'   (e.g., \code{pH_units}, \code{temp_C}, \code{sp_conductivity_uScm}).
#'
#' @param min_secs Minimum duration (seconds) of observations required to compute
#'   stability statistics. This value is converted internally to the minimum
#'   number of observations required in a stationary block. Default is \code{5}.
#'
#' @param slope_thresh Maximum allowable absolute slope (units per minute) for
#'   stable measurements. Default is \code{0.05}.
#'
#' @param range_thresh Maximum allowable range (max - min) of values within the
#'   evaluation window for stability. Default is \code{0.02}.
#'
#' @param stationary_thresh Minimum value of \code{is_stationary_status} (seconds)
#'   used to define stationary periods to evaluate. Default is \code{998}.
#'
#' @param drop_cols Logical. If \code{TRUE} (default), intermediate diagnostic
#'   columns used during slope and range calculations are removed from the
#'   returned data frame.
#'
#' @param verbose Logical. If \code{TRUE}, prints the depths where stationary
#'   blocks were identified.
#'
#' @param plot Logical. If \code{TRUE}, produces a diagnostic stability plot
#'   using \code{plot_stability()}.
#'
#' @details
#' The function requires the following columns in \code{df}:
#'
#' \itemize{
#'   \item \code{DateTime}
#'   \item \code{depth_m}
#'   \item \code{stationary_depth}
#'   \item \code{stationary_block_id}
#'   \item \code{is_stationary_status}
#'   \item the column specified by \code{value_col}
#' }
#'
#' Observations containing \code{NA} in \code{value_col} are excluded from
#' stability calculations.
#'
#' A new logical column is added to the output named
#' \code{<value_col>_stable}, indicating whether each observation meets
#' the stability criteria.
#'
#' @return
#' A data frame with the original input data plus a logical stability flag
#' column named \code{<value_col>_stable}. If \code{drop_cols = FALSE},
#' additional diagnostic columns describing slope, range, and window
#' metadata are included.
#'
#' @examples
#' \dontrun{
#' dat <- is_stationary(sensor_data)
#'
#' dat <- TROLL_sensor_stable(
#'   df = dat,
#'   value_col = pH_units,
#'   min_secs = 5,
#'   slope_thresh = 0.05,
#'   range_thresh = 0.02
#' )
#' }
#'
#' @seealso
#' \code{\link{is_stationary}}
#'
#' @export
TROLL_sensor_stable <- function(df,
                                value_col = pH_units,
                                min_secs = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                                slope_thresh = 0.05,     # units/minute
                                range_thresh = 0.02,
                                stationary_thresh = 998,
                                drop_cols = TRUE,
                                verbose = FALSE,
                                plot = FALSE){
  # 0. --- Tidy Eval --- ----
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)
  value_flag_col <- paste0(value_name,"_stable")
  # 1. --- Input/Validation checks --- ----

  # Check required columns exist
  req_cols <- c('DateTime','depth_m','stationary_depth','stationary_block_id','is_stationary_status',value_name)

  if(!all(req_cols %in% names(df))){
    missingCols <- req_cols[!req_cols %in% names(df)]

    # Add indentation to each column name and join with newlines
    missing_list <- paste0("  - ", missingCols, collapse = "\n")

    stop(paste0("Missing columns:\n", missing_list,
                '\n\nBe sure to run "is_stationary" first to identify stationary blocks.'))
  }

  # Check min_secs and slope_thresh are positive numeric
  if( !is.numeric(min_secs) || !is.numeric(slope_thresh) ){
    stop('\nBoth "min_secs" and "slope_thresh" must be positive numeric values.')
  }
  if(!(min_secs >= 0 && slope_thresh >= 0)){
    stop('\nBoth "min_secs" and "slope_thresh" must be positive numeric values.')
  }

  # Check that stationary blocks exist above the stationary_thresh
  if(!is.numeric(stationary_thresh) || stationary_thresh <= 0){
    stop('\n "stationary_thresh must be a positive numeric greater than 0')
  }
  z_stationary <- unique(df$stationary_depth[df$is_stationary_status > stationary_thresh])

  if(length(z_stationary) < 1){
    max_stnthresh <- max(df$is_stationary_status,na.rm = T)
    stop(paste0('No stationary data identified using given "stationary_thresh" value.\n',
                'Max "is_stationary_status = ', max_stnthresh,' seconds.'))
  }

  if(verbose){
    message(paste0('Stationary depths at:\n',
                   paste0(round(z_stationary,2),collapse = '\n'),
                   '\nfound in the data'))
  }

  # 2. --- Calculate min_obs to keep --- ----
  samp_int <- get_sample_interval(datetime_data = df$DateTime, output_units = 'secs',tol_prop = 1)

  min_obs <- ceiling(min_secs / samp_int)
  # Check that enough data points in every stationary block to accomodate min_obs
  blocksizes <- df |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::filter(is_stationary_status > stationary_thresh) |>
    dplyr::mutate(
      block_n = dplyr::n()
    ) |>
    dplyr::pull(block_n) |>
    unique()

  # Strict stop for now, could update to use full block instead with warning later if desired (already coded, commented out)
  if(any(blocksizes < min_obs)){
    stop(paste0('\nToo few observations exist in at least one stationary block to accomodate "min_secs" requirement.\n',
         'Reduce "min_secs" to be below the shortest stationary block: \n',
         min(blocksizes,na.rm = TRUE)*samp_int))
  }

  # 3. --- Filter data into stationary blocks --- ----
  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status > stationary_thresh,
      !is.na(.data[[value_name]])  # remove rows with NA in the value_col data
    )
  # 4. --- Group data into blocks, and split groups into a list --- ----

  dat_grouped <- dat_base |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")) # Standardize time into minutes from start of data
    ) |>
    dplyr::group_split()
  # 5. --- Compute range and slope over stationary blocks --- ----
  # Pre-allocate output object
  out_list <- vector("list", length(dat_grouped))

  for (i in seq_along(dat_grouped)) { # seq_along gives list length as 1:length(list)
    #  **** Extract one stationary group of data within a loop ----
    stable_group_dat <- dat_grouped[[i]]                    # Extract one stable group of data

    n <- nrow(stable_group_dat)                             # length of the stable block (rows)

    # Stop execution if block is empty
    if(n==0){
      msg_depth <- unique(stable_group_dat$stationary_depth)
      stop(paste0('Zero data returned for depth ',msg_depth))
    }

    # **** Pre-allocate group output dataframe ----
    group_out <- tibble::tibble(
      stationary_block_id = stable_group_dat$stationary_block_id,
      DateTime = stable_group_dat$DateTime,
      n_used = NA_integer_,
      slope = NA_real_,
      range = NA_real_,
      n_dropped = NA_integer_,
      slope_ok = FALSE,
      range_ok = FALSE
    )

    # Add sensor flag where both slope/range thresholds are met, This dynamically allows different sensor names
    group_out[[value_flag_col]] <- FALSE

    # **Reset good_flag per group** (marker to keep final "_stable" flat as TRUE once one row meets range + slope thresholds combined)
    good_flag <- FALSE

    # Set min_obs rows slope and range OK flags to keep data if threshold is never met
    if(nrow(group_out) < min_obs){
      msg_depth <- unique(stable_group_dat$stationary_depth)
      warning(paste0('The given "min_obs" is larger than the number of stable data identified for ',value_name,' at',msg_depth,'.'))
    }

    tailstart <- max(1, nrow(group_out) - min_obs + 1) # Number of rows to keep if min_obs only

    group_out$slope_ok[tailstart:nrow(group_out)] <- TRUE
    group_out$range_ok[tailstart:nrow(group_out)] <- TRUE
    group_out[[value_flag_col]][tailstart:nrow(group_out)] <- TRUE

    # assign sensor data and time to variables
    x_all <- stable_group_dat$t
    y_all <- stable_group_dat[[value_name]]

    # Create a safety net in case the # of data rows < min_obs
    # if(n < min_obs) {  # If fewer rows of data exist than the min_obs as specified
    #   n_windows <- n # Use all available data to compute statistics (slope, range)
    #   msg_depth <- unique(stable_group_dat$stationary_depth)
    #   warning(paste0('The given "min_obs" is larger than the number of stable data identified for ',value_name,' at',msg_depth,'.'))
    # } else {         # Or if more data exist than min_obs rows
    #   n_windows <- n - min_obs + 1 # Evaluate stats for the number of rows (n) minus the tail (min_obs)
    # }

    n_windows <- n - min_obs + 1 # Evaluate stats for the number of rows (n) minus the tail (min_obs)

    # **** Loop over grouped data ----
    for(win_start in seq_len(n_windows)) {                  # Because seq_along(0) = 1, this always works
      # Create an index to subset group data on from the start of the window to the end of the stable data block
      idx <- win_start:n

      # Subset data using window index
      x <- x_all[idx]
      y <- y_all[idx]

      # **** --- Calculate stats --- ----

      # Slope
      v <- stats::var(x)

      slope_fit <- if(is.na(v) || v == 0) {
        NA_real_
      } else {
        stats::cov(x, y) / v
      }

      if(is.na(slope_fit)){
        warning(paste0("Single datapoint used for ",unique(stable_group_dat$stationary_depth),", slope could not be calculated"))
      }

      # Range
      win_range <- diff(range(y))

      # Add stats and metadata to output object at the start index for grouped data
      group_out$slope[win_start] <- slope_fit
      group_out$range[win_start] <- win_range

      group_out$n_dropped[win_start] <- win_start - 1
      group_out$n_used[win_start] <- length(idx)

      # Flag columns that meet slope/range
      slope_good <- !is.na(slope_fit) && abs(slope_fit) <= slope_thresh
      if(slope_good) {
        group_out$slope_ok[win_start] <- TRUE
      }

      range_good <- win_range <= range_thresh
      if(range_good){
        group_out$range_ok[win_start] <- TRUE
      }

      # Combined flag if both conditions are met
      if((slope_good && range_good) || good_flag){
        group_out[[value_flag_col]][win_start] <- TRUE
        good_flag <- TRUE
      }

    }
    out_list[[i]] <- group_out
  }


  # . Compile data output --- ----
  # Compile the group_out into a dataframe
  out <- dplyr::bind_rows(out_list)

  # join the out metadata with the input dataframe
  final <- df |>
    dplyr::left_join(out, by = c("stationary_block_id","DateTime"))

  # Drop optional columns
  if(drop_cols){
    final <- final |>
      dplyr::select(-c(n_used,slope,range,n_dropped,slope_ok,range_ok))
  }

  # Put the flag next to the sensor data column
  final <- final |>
    dplyr::relocate(all_of(value_flag_col), .after = all_of(value_name))

  # 6. --- Optional Plotting --- ----
  if(plot == TRUE){
    plot_stability(df = final,
                   value_col_sym = value_col,
                   value_flag_col = value_flag_col,
                   range_thresh = range_thresh)
  }

  return(final)
}


xx <-
  TROLL_sensor_stable(df = dat_stationary,
                      value_col = sp_conductivity_uScm,
                      stationary_thresh = 998,
                      min_secs = 5,
                      slope_thresh = 0.05,
                      range_thresh = 0.1,
                      drop_cols = TRUE,
                      verbose = FALSE,
                      plot = TRUE);xx

# Need to deal with y axis scaling when it blows up


