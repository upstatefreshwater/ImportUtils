#' Plot Sensor Stability Diagnostics
#'
#' Generates a time series plot of a single sensor parameter, highlighting
#' stationary and stability states as identified by \code{is_stationary()}
#' and \code{TROLL_sensor_stable()}.
#'
#' Observations are colored by profiling state:
#' \itemize{
#'   \item \code{"Sonde Moving"}: Sonde is in motion
#'   \item \code{"Unstable Stationary"}: Stationary but stability criteria not met
#'   \item \code{"Stable Stationary"}: Meets stability criteria
#'   \item \code{"Trimmed (Settling)"}: Removed from analysis due to settling time
#' }
#'
#' If \code{range_thresh} is provided, horizontal dashed lines are drawn
#' around the median value to visualize the allowable range threshold.
#'
#' @param df A data frame containing at minimum:
#'   \itemize{
#'     \item \code{DateTime}
#'     \item the sensor column specified by \code{value_col_sym}
#'     \item a logical stability column (\code{value_flag_col})
#'     \item \code{is_stationary_status}
#'   }
#'
#' @param value_col_sym Unquoted name of the sensor column to plot.
#'
#' @param value_flag_col Character. Name of the logical column indicating
#'   stability (typically \code{"<param>_stable"}).
#'
#' @param range_thresh Numeric. Range threshold used for stability detection.
#'   If \code{NA}, no threshold lines are drawn.
#'
#' @return The plot is printed.
#'
#' @details
#' This function is primarily intended for diagnostic visualization during
#' tuning of stability parameters.
#'
#' @seealso \code{\link{TROLL_sensor_stable}}, \code{\link{is_stationary}}
#'
#' @keywords internal
plot_stability <- function(df,
                           value_col_sym,
                           value_flag_col,
                           range_thresh
                           ){

  # Standardize data for plotting
  plt_df <- tibble::tibble(
    DateTime = df$DateTime,
    value = df[[rlang::as_name(value_col_sym)]],
    stable = df[[value_flag_col]],
    is_stationary_status = df$is_stationary_status
  )

  plt_df <- plt_df |>
    dplyr::mutate(
      state = dplyr::case_when(
        is_stationary_status == 888 ~ "Trimmed (Settling)",
        is_stationary_status == 999 & stable ~ "Stable Stationary",
        is_stationary_status == 999 & !stable ~ "Unstable Stationary",
        TRUE ~ "Sonde Moving"
      )
    )

  # value_flag_sym <- rlang::sym(value_flag_col)

  # Starter to place a horizontal line for visualizing range threshold on the plot at the median so one line at least always shows up within the plot window
  if(!is.na(range_thresh)){
    rangelines <- stats::median(plt_df$value ,na.rm = TRUE)
    rangelines <- c(rangelines - 0.5*range_thresh,
                    rangelines + 0.5*range_thresh)
  }

  p1 <-
    #############
  ggplot2::ggplot(plt_df, ggplot2::aes(DateTime, value, color = state)) +
    ggplot2::geom_line(color = "grey70", linewidth = 0.4) + # Decouple from grouping so line goes through all data
    ggplot2::geom_point(size = 1.25) +
    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "Sonde Moving" = "grey70",
        "Stable Stationary" = "#440154FF",# "dodgerblue3",
        "Trimmed (Settling)" = "#FDE725FF",
        "Unstable Stationary" = "firebrick1"
      )
    ) +

    # Add a lines representing the range threshold
    # ggplot2::geom_hline(ggplot2::aes(yintercept = rangelines[1], linetype = 'Range Threshold'),
    #                     color = 'gray30') +
    # ggplot2::geom_hline(ggplot2::aes(yintercept = rangelines[2], linetype = 'Range Threshold'),
    #                     color = 'gray30') +
    # ggplot2::scale_linetype_manual(name = NULL,
    #                                values = 2) +
    # Add titles
    ggplot2::labs(x = 'Time of Day',
                  y = rlang::as_name(value_col_sym)) +

    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.box.just = "left",      # Keeps legends aligned even if widths differ
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.y = ggplot2::unit(0.1, "cm"), # Small positive value prevents overlap
      legend.background = ggplot2::element_blank()
    )

  if(!is.na(range_thresh)){
    p1 <- p1 +
      ggplot2::geom_hline(ggplot2::aes(yintercept = rangelines[1], linetype = 'Range Threshold'),
                          color = 'gray30') +
      ggplot2::geom_hline(ggplot2::aes(yintercept = rangelines[2], linetype = 'Range Threshold'),
                          color = 'gray30') +
      ggplot2::scale_linetype_manual(name = NULL,
                                     values = 2)
  }

  print(p1)
}

# Helper for identifying optical data
is_optical <- function(name){
  name %in% c('turbidity_NTU','chlorophyll_RFU','bga_fluorescence_RFU')
}


#' Identify Stable Sensor Values Within Stationary Profiling Blocks
#'
#' Evaluates whether sensor measurements collected during stationary profiling
#' periods are stable based on slope and range thresholds. Stability is assessed
#' within each stationary block identified by \code{is_stationary()}, and the
#' function flags observations that meet both slope and range criteria.
#'
#' @details
#'
#' For each stationary block, slope (units per minute) and value range are
#' calculated iteratively using progressively smaller windows, beginning with
#' the full stationary block and removing leading observations.
#'
#' A minimum tail duration defined by \code{min_median_secs} is always retained.
#' The function searches for the earliest point in the block where both
#' \code{slope_thresh} and \code{range_thresh} are satisfied, and flags that
#' observation and all subsequent observations as stable.
#'
#' For optical parameters (e.g., turbidity, chlorophyll, fluorescence),
#' all observations within stationary periods (after settling removal) are
#' flagged as stable. Range-based criteria are not applied, and slope is used
#' only to generate diagnostic warnings.
#'
#' For non-optical sensors, the function searches until `slope_thresh` and
#' `range_thresh` are both met concurrently, and marks the remainder of the
#' stationary block at "stable". If neither threshold can be met, the final
#' \code{min_median_secs} of data within the stationary block are retained and
#' flagged as stable.
#'
#' The input data frame must contain the following columns:
#' \itemize{
#'   \item \code{DateTime}
#'   \item \code{depth_m}
#'   \item \code{stationary_depth}
#'   \item \code{stationary_block_id}
#'   \item \code{is_stationary_status}
#'   \item the column specified by \code{value_col}
#' }
#'
#' Observations with \code{NA} values in \code{value_col} are excluded
#' from stability calculations.
#'
#' This function requires that stationary blocks have already been identified
#' using \code{\link{is_stationary}}.Stability calculations are performed only on
#' observations where \code{is_stationary_status == 999}, i.e., fully stationary data
#' after removal of the initial settling period. The function will error if any
#' stationary block does not contain sufficient duration (after settling removal)
#' to satisfy \code{min_median_secs}.
#'
#' @param df A data frame containing sensor data and stationary block metadata.
#'
#' @param value_col Unquoted name of the sensor column to evaluate for stability
#'   (e.g., \code{pH_units}, \code{temp_C}, \code{sp_conductivity_uScm}).
#'
#' @param min_median_secs Numeric. Minimum duration (seconds) of data to retain
#'   at the end of each stationary block. If stability criteria are not met,
#'   this trailing window is still flagged as stable and used for summary
#'   calculations. Default is \code{5}.
#'
#' @param slope_thresh Numeric. Maximum allowable absolute slope (units per minute)
#'   for stable measurements. If \code{NULL}, a parameter-specific default is
#'   retrieved from \code{stability_ranges}.
#'
#' @param range_thresh Numeric. Maximum allowable range (max - min) within the
#'   evaluation window. If \code{NULL}, a parameter-specific default is retrieved
#'   from \code{stability_ranges}.
#'
#' @param settling_secs Numeric. Number of seconds to remove from the start
#'   of each stationary block to allow sensor equilibration. These observations
#'   are flagged with \code{is_stationary_status = 888}.
#'
#' @param drop_cols Logical. If \code{TRUE} (default), intermediate diagnostic
#'   columns used during slope and range calculations are removed from the
#'   returned data frame.
#'
#' @param verbose Logical. If \code{TRUE}, prints the depths where stationary
#'   blocks were identified.
#'
#' @param plot Logical. If \code{TRUE}, generates a diagnostic plot using
#'   \code{plot_stability()}. Intended for tuning and debugging, not
#'   for final analysis outputs.
#'
#' @return
#' A data frame containing the original input data with an added logical column
#' \code{<value_col>_stable}. This column indicates whether each observation
#' meets the stability criteria.
#'
#' If \code{drop_cols = FALSE}, additional diagnostic columns are included:
#' \itemize{
#'   \item \code{slope}: slope of the evaluation window
#'   \item \code{range}: value range within the window
#'   \item \code{n_used}: number of observations used
#'   \item \code{n_dropped}: number of leading observations removed
#'   \item \code{slope_ok}, \code{range_ok}: logical threshold checks
#' }
#'
#' @examples
#' \dontrun{
#' dat <- is_stationary(sensor_data)
#'
#' dat <- TROLL_sensor_stable(
#'   df = dat,
#'   value_col = pH_units,
#'   min_median_secs = 5,
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
                                min_median_secs = 4,            # minimum time required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                                slope_thresh = NULL,     # units/minute
                                range_thresh = NULL,
                                settling_secs = 10,
                                drop_cols = TRUE,
                                verbose = FALSE,
                                plot = FALSE){

  # 0. --- Tidy Eval --- ----
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)
  value_flag_col <- paste0(value_name,"_stable")

  optical_param <- is_optical(value_name)

  # 00.--- Use default slope/range thresholds if NULL arg --- ----
  # - Slope
  if(is.null(slope_thresh)){
    # - Safe join filter to extract from internal data
    match_idx <- which(stability_ranges$param == value_name)

    if(length(match_idx) == 0){
      stop(paste0("No slope threshold found for ", value_name))
    }

    # If exists in internal data extract
    slope_thresh <- stability_ranges$slope[match_idx[1]] # Take the first match
  }

  # - Range
  if(is.null(range_thresh)){
    # - Safe join filter to extract from internal data
    match_idx <- which(stability_ranges$param == value_name)

    if(length(match_idx) == 0){
      stop(paste0("No range threshold found for ", value_name))
    }

    range_thresh <- stability_ranges$range[match_idx[1]] # Take the first match

    # Add na.rm = TRUE to the max calculation
    max_val <- max(df[[value_name]], na.rm = TRUE)

    # Guard against -Inf or NA from the data
    if(!is.infinite(max_val) && !is.na(max_val)){
      range_thresh <- min(range_thresh, max_val * 0.8)
    }
  }

  # Override range_thresh for optical params
  if(optical_param){
    range_thresh <- NA
  }
  # 1. --- Input/Validation checks --- ----

  # Re-make stationary_block_id if status and depth are present
  if(!'stationary_block_id' %in% names(df) && 'is_stationary_status' %in% names(df)){
    df <- df |>
      dplyr::mutate(stationary_block_id = dplyr::consecutive_id(is_stationary_status))
  }

  # Check required columns exist
  req_cols <- c('DateTime','depth_m','stationary_depth','stationary_block_id','is_stationary_status',value_name)

  if(!all(req_cols %in% names(df))){

    missingCols <- req_cols[!req_cols %in% names(df)]

    # Add indentation to each column name and join with newlines
    missing_list <- paste0("  - ", missingCols, collapse = "\n")

    stop(paste0("Missing columns:\n", missing_list,
                '\n\nBe sure to run "is_stationary" first to identify stationary blocks.'))
  }

  # Check min_median_secs and slope_thresh are positive numeric
  if(!is.numeric(min_median_secs) || length(min_median_secs) != 1 || is.na(min_median_secs) || min_median_secs < 0){
    stop('`min_median_secs` must be a single positive numeric value.')
  }

  if(!is.numeric(slope_thresh) || length(slope_thresh) != 1 || is.na(slope_thresh) || slope_thresh < 0){
    stop('`slope_thresh` must be a single positive numeric value.')
  }

  if(!optical_param){
    if(!is.numeric(range_thresh) || length(range_thresh) != 1 || is.na(range_thresh) || range_thresh < 0){
      stop('`range_thresh` must be a single positive numeric value.')
    }
  }

  # Check that stationary blocks exist with fully stationary status (999)
  z_stationary <- unique(df$stationary_depth[df$is_stationary_status == 999])

  if(length(z_stationary) < 1){
    max_stnthresh <- max(df$is_stationary_status, na.rm = TRUE)
    stop(paste0('No stationary data blocks (stationary_status 999) found.\n',
                'Max "is_stationary_status = ', max_stnthresh,' seconds.\n\n',
                'Try reducing `stationary_secs` in is_stationary() if your deployment had shorter soak times.'))
  }

  if(verbose){
    message(paste0('Stationary depths at:\n',
                   paste0(round(z_stationary,2),collapse = '\n'),
                   '\nfound in the data'))
  }


  # 2. --- Trim off settling time from stationary blocks --- ----

  # Use time based trimming to avoid issues with bluetooth glitches
  df <- df |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      # Mark stationary block start time for fully stationary blocks only
      block_start_time = {
        idx <- is_stationary_status == 999
        if (any(idx)) min(DateTime[idx]) else as.POSIXct(NA)
      },
      # Calculate actual time since start of block
      time_since_start = as.numeric(difftime(DateTime, block_start_time, units = "secs")),
      # Change the stationary flag to 888 to indicate trimming occurred
      is_stationary_status = dplyr::if_else(
        is_stationary_status == 999 & time_since_start < settling_secs,
        888,
        is_stationary_status
      )
    ) |>
    dplyr::ungroup()


  # Check for enough time in every stationary block to accommodate min_median_secs
  block_durations <- df |>
    dplyr::filter(is_stationary_status == 999) |> # Only use fully stationary (post-settling) data for stability calculations
    dplyr::group_by(stationary_block_id) |>
    dplyr::summarise(
      duration = as.numeric(max(DateTime) - min(DateTime), units = "secs")
    ) |>
    dplyr::pull(duration)

  # Strict stop for now, could update to use full block instead with warning later if desired
  if(any(block_durations < min_median_secs)){
    stop("Stationary block too short in duration...")
  }

  # 3. --- Filter data into stationary blocks --- ----

    #**** 3a. Possible add logic###############
  # If we want to make this dependent on the parameter type (trim settling out only for optical params,
  # and let range + slope thresholds decide for others), this would be the place to add that logic.

  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status == 999,
      !is.na(.data[[value_name]])  # remove rows with NA in the value_col data
    )
  # 4. --- Group data into blocks, and split groups into a list --- ----

  dat_grouped <- dat_base |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")) # Standardize time into minutes from start of data
    ) |>
    dplyr::group_split()

  # Pre-allocate output object and warnings collector
  out_list <- vector("list", length(dat_grouped))

  slope_issues <- list()
  # >> Loop through each stationary block ----
  for (i in seq_along(dat_grouped)) {# seq_along gives list length as 1:length(list)
    # **** Extract one stationary group of data within a loop ----
    stable_group_dat <- dat_grouped[[i]]                    # Extract one stationary block of data

    n <- nrow(stable_group_dat)                             # length of the stationary block (rows)

    # Stop execution if block is empty
    if(n==0){
      msg_depth <- unique(stable_group_dat$stationary_depth)
      stop(paste0('Zero data returned for depth ',msg_depth))
    }

    # Stationary block depth
    depth_vals <- unique(stable_group_dat$stationary_depth[!is.na(stable_group_dat$stationary_depth)])

    if(length(depth_vals) != 1){
      stop("stationary_depth is not constant within a block.")
    }

    block_depth <- round(depth_vals, 2)



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

    # assign sensor data and time to variables
    x_all <- stable_group_dat$t
    y_all <- stable_group_dat[[value_name]]

    # Compute overall slope for optical params
    if(optical_param){
      full_slope <- calc_slope(x = x_all, y = y_all)
      #   {
      #   x <- x_all
      #   y <- y_all
      #   v <- stats::var(x)
      #   if(is.na(v) || v == 0) NA_real_ else stats::cov(x, y) / v
      # }
    }

    # **** Flag the tail min_median_secs as "_stable" ----
    end_time <- max(x_all) # extract the end time of the grouped data

    # create an index of the minimum tail to keep based on time
    valid_tail <- which((end_time - x_all) * 60 <= min_median_secs)

    if(length(valid_tail) == 0){
      next
    }
    # Force contiguous tail if ever timestamps got messed up
    valid_tail <- seq.int(min(valid_tail), n)

    # Change the "_stable" flag to TRUE for those observations to meet min_median_secs input
    group_out[[value_flag_col]][valid_tail] <- TRUE

    # Locate the index of the first row kept by force at the tail of the stationary period
    tailstart <- match(TRUE, group_out[[value_flag_col]])

    # Guard against possible empty tails
    if(is.na(tailstart)){
      next  # Could swith this to stop if we want to be strict
    }

    # Extract the number of windows to iterate over (data can shrink down to only the length of the tail while searching for slope/range thresholds)
    n_windows <- tailstart - 1

    # >> Loop that iteratively shrinks each block, calculating stats ----

    for(win_start in seq_len(n_windows)) {
      # Create an index to subset group data on from the start of the window to the end of the stable data block
      # **NOTE** this encompasses all group data including the tail!!!!
      idx <- win_start:n

      # Subset data using window index
      x <- x_all[idx]
      y <- y_all[idx]

      # **** --- Calculate stats --- ----

      # Slope (ALWAYS CALCULATE SLOPE)
      slope_fit <- calc_slope(x=x,y=y)

      # Range

      win_range <- max(y) - min(y) #diff(range(y))

      # Add stats and metadata to output object at the start index for grouped data
      group_out$slope[win_start] <- slope_fit
      group_out$range[win_start] <- win_range

      group_out$n_dropped[win_start] <- win_start - 1
      group_out$n_used[win_start] <- length(idx)

      # Flag columns that meet slope/range
      slope_good <- !is.na(slope_fit) && abs(slope_fit) <= slope_thresh
      group_out$slope_ok[win_start] <- slope_good

      range_good <- win_range <= range_thresh
      group_out$range_ok[win_start] <- range_good
    } # > End of shrinking group loop ----

    # For optical params, warn if slope exceeds threshold, but do not error
    if(optical_param){

      if(!is.na(full_slope) && abs(full_slope) > slope_thresh){

###########
        slope_issues[[length(slope_issues) + 1]] <- list(
          depth = block_depth,
          slope = full_slope
        )
      }
    }
  # 6. --- Flag "_stable" observations --- ----
    # Flag all non-optical parameters as "_stable" after slope + range threshold are met once
    if(!optical_param){
      # Extract locations where both slope and range thresholds were met simultaneously (if any)
      stbl_idx <- which(group_out$slope_ok & group_out$range_ok)

      # Update "_stable" flags (NOTE: if slope + range criteria were not met, this reverts to tail only)
      if(length(stbl_idx) > 0){
        first_valid <- min(stbl_idx)
        group_out[[value_flag_col]][first_valid:n] <- TRUE
      }
    # Flag all stationary for optical parameters
    } else{
      group_out[[value_flag_col]] <- TRUE
    }

    # 7. --- Assign output to object --- ----
    out_list[[i]] <- group_out
  } # >End of stationary block loop -----
  # Slopes warning
  if(length(slope_issues) > 0){

    depths <- vapply(slope_issues, `[[`, numeric(1), "depth")
    slopes <- vapply(slope_issues, `[[`, numeric(1), "slope")

    msg <- paste0(
      value_name, " slope exceeded threshold at:\n",
      paste0("  ", round(depths, 2), " m (", round(slopes, 3), " units/min)", collapse = "\n"),
      "\n\nConsider validating data or adjusting trimming."
    )

    warning(msg)
  }
  # 6. --- Compile data output --- ----
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
    dplyr::relocate(all_of(value_flag_col), .after = all_of(value_name)) |>
    dplyr::ungroup()

  # 7. --- Optional Plotting --- ----
  if(isTRUE(plot)){
    plot_stability(df = final,
                   value_col_sym = value_col,
                   value_flag_col = value_flag_col,
                   range_thresh = range_thresh)
  }

  return(final)
}




