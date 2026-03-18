#' Identify Stationary Periods in Depth Data
#'
#' This function flags periods where a sonde or depth sensor is effectively stationary based on a rolling range of depth measurements.
#' It can handle irregular sampling intervals, optionally trims the start of stationary blocks, and optionally plots
#' depth and rolling range over time.
#'
#' @param df A data frame containing depth and datetime observations.
#' @param depth_col Column in `df` representing depth measurements (numeric). Default is `depth_m`.
#' @param datetime_col Column in `df` representing observation timestamps (POSIXt). Default is `DateTime`.
#' @param depth_range_threshold Numeric. Maximum depth range within a rolling window to consider the sonde stationary. Default is `0.05`.
#' @param stationary_secs Numeric. Minimum duration in seconds for a block to be considered stationary. Default is `60`.
#' @param rolling_range_secs Numeric. Window size in seconds used to calculate rolling depth range. Default is `10`.
#' @param start_trim_secs Numeric. Number of seconds to trim from the start of stationary blocks to remove initial jumps. Default is `15`.
#' @param drop_cols Logical. If TRUE, intermediate columns used for computation are removed from the output. Default is `TRUE`.
#' @param plot Logical. If TRUE, produces plots showing depth and rolling range with stationary periods highlighted. Default is `FALSE`.
#'
#' @return A data frame identical to `df` but with additional columns (provided `drop_cols` is \code{TRUE}:
#'   \describe{
#'     \item{is_stationary_status}{Numeric. 999 = fully stationary block, intermediate values = partial stationary duration in seconds, 0 = not stationary.}
#'     \item{stationary_depth}{Mean depth during stationary blocks (NA if not stationary).}
#'   }
#'
#'   If `drop_cols` is \code{FALSE}, intermediate columns are retained in addition to \code{is_stationary_status, stationary_depth}:
#'   \describe{
#'   \item{is_stationary_initial}{Logical. TRUE = met rolling range criteria after `start_trim_secs` was applied, prior to application of `stationary_secs`.}
#'   \item{stationary_block_id}{Numeric. Consecutive id's of data blocks, including non-stationary blocks}
#'   \item{block_n}{Numeric. The number of observations in each stationary block}
#'   \item{block_secs}{Numeric. Elapsed time in seconds of each stationary block.}
#'   }
#'
#' @details
#' The function first calculates the sampling interval based on lagged differences of the `datetime_col`.
#' It then computes a rolling range of depth values across a window size of length = `rolling_range_secs`.
#' It identifies candidate stationary periods where the rolling range is below `depth_range_threshold`.
#' The `start_trim_secs` parameter enables trimming of initial observations in a stationary block.
#' Blocks shorter than `stationary_secs` are flagged with `is_stationary_status` equal to seconds stationary.
#' Blocks longer than `stationary_secs` are flagged as 999. If the sampling interval exceeds 30 seconds,
#' the sonde is assumed to be fixed, and all rows are returned with `is_stationary_status = 999`.
#'
#' @seealso \code{\link[zoo]{rollapply}}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- data.frame(
#' DateTime = seq.POSIXt(from = as.POSIXct("2026-01-01 00:00"),
#'                       by = "sec", length.out = 170),
#' depth_m = c(rep(1.0, 50), rep(1.05, 70), rnorm(50,2,.02))
#' )
#' df_flagged <- is_stationary(df, depth_col = depth_m, datetime_col = DateTime,
#'                             start_trim_secs = 5, stationary_secs = 45,
#'                             depth_range_threshold = 0.05)
#' }
#'
#'
#' @export
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          stationary_secs = 60,
                          rolling_range_secs = 10,
                          start_trim_secs = 15,
                          drop_cols = TRUE,
                          plot = FALSE) {
  # 00. --- rlang arguments --- ----
  depth_col <- rlang::enquo(depth_col)
  datetime_col <- rlang::enquo(datetime_col)

  # 0. --- Checks --- ----
  # df columns exist
  missing_cols <- setdiff(c(rlang::as_name(depth_col),
                            rlang::as_name(datetime_col)),
                          names(df))
  if(length(missing_cols) > 0) {
    stop("Required columns missing from input data:\n",
         paste(missing_cols, collapse = ", "))
  }

  # df columns formatting

  if (!is.numeric(df[[rlang::as_name(depth_col)]]) ||
      !inherits(df[[rlang::as_name(datetime_col)]], "POSIXt")) {
    stop('Required columns are incorrectly formatted.\nCheck that "depth_col" is numeric, and "datetime_col" is a POSIXt.')
  }

  # Check on sampling interval
  samp_int <- get_sample_interval(df[[rlang::as_name(datetime_col)]]) # this will warn if multiple sampling intervals exist

    # Check that the start_trim_secs is large enough to

  # Check that user didn't accidentally trim more data than stationary_secs
  if(start_trim_secs >= stationary_secs){
    stop('"start_trim_secs" must be smaller than "stationary_secs"')
  }

  # 0-a. --- If sonde fixed in position, return data with is_stationary_status == 999 for all rows ---
  if(samp_int > 30){
    # Would be good to automate a depth check in here
    message("Sampling interval > 30s detected. Sonde assumed fixed in position.")
    return(df |> dplyr::mutate(is_stationary_status = 999))
  }

  # 1. --- Create objects needed for flagging stationary blocks --- ----
  stationary_n <- ceiling(stationary_secs / samp_int)                           # The number of obs needed to be considered fully stationary
  rolling_n <- ceiling(rolling_range_secs / samp_int)                           # The number of obs needed for the rolling range window
  trim_n <- max(1,ceiling(start_trim_secs / samp_int))                          # The number of obs to trim off the start of a stationary block (force at least 1)
  start_n_trimmed <- rolling_n - trim_n                                         # The number of obs to flag as stationary after trimming at the start of stationary blocks


  # return(start_n_trimmed)

  # If window is more than the number of rows in the dataset
  if(rolling_n > nrow(df)){
    rolling_n <- nrow(df)
    warning(paste('Number of rows in data exceeds the number of observations required for rolling range calculation./n
                  Range calculation will use the maximum # of rows:', rolling_n))
  }

  # 2. --- Compute rolling range --- ----
  # Pull depth values
  depth_vals <- df[[rlang::as_name(depth_col)]]

  # Rolling min and max
  roll_min <- zoo::rollapply(depth_vals, width = rolling_n, FUN = min, fill = NA, align = "right")
  roll_max <- zoo::rollapply(depth_vals, width = rolling_n, FUN = max, fill = NA, align = "right")
  roll_range <- roll_max - roll_min

  # 3. --- Create initial stationary flag and an index for stationary obs based on the rolling window --- ----
  # Initialize stationary flag as all FALSE
  is_stationary_flag <- rep(FALSE, length(depth_vals))

  # Create an index of positions where the rolling range is below the "depth_range_threshold"
  good_rollrange_idx <-  which(!is.na(roll_range) & roll_range < depth_range_threshold) #

  # Update stationary flag with TRUE locations
  # Mark those locations from the rolling range threshold index as TRUE
  is_stationary_flag[good_rollrange_idx] <- TRUE                                 # This has to be done before trimming!!!

  # 4. --- Apply trimming logic --- ----
  # Create a boolean where rolling range below the range threshold is TRUE
  bool_roll <- !is.na(roll_range) & roll_range < depth_range_threshold          # Avoid NA propagation issues

  # This utility function returns indices of stable observations to be trimmed from the start of the stable period
  trim_idx <- trim_stationary_starts(range_met_vector = bool_roll,
                                     rolling_n = rolling_n,
                                     depth_range_threshold = depth_range_threshold,
                                     trim_n = trim_n)

  # Update the stationary_flag to include trimming
  # If the n_obs to be trimmed is less than the rolling window,
  # shift the difference of obs to TRUE from the start_idx backwards
  if(trim_n < rolling_n){
    is_stationary_flag[trim_idx] <- TRUE

    #If there are more n_obs to be trimmed than the size of the rolling window,
    # convert the difference to FALSE between start_idx:trim_n
  } else{
    is_stationary_flag[trim_idx] <- FALSE
  }

  # 5. --- Calculate stationary block duration --- ----
  stationary_block_id <- dplyr::consecutive_id(is_stationary_flag)
  df_out <- df |>
    dplyr::mutate(
      is_stationary_initial = is_stationary_flag, #
      stationary_block_id = stationary_block_id   # For grouping (includes both T/F blocks)
    ) |>
    dplyr::group_by(stationary_block_id, is_stationary_initial) |> # Include both so computation is done by T/F grouping on stationary_flag
    dplyr::mutate(
      block_n = dplyr::n(),
      block_secs = as.numeric(max({{datetime_col}}) - min({{datetime_col}}), units = "secs"),
      is_stationary_status = dplyr::case_when(
        is_stationary_initial & block_secs >= stationary_secs ~ 999,
        is_stationary_initial & block_secs < stationary_secs ~ block_secs,
        TRUE ~ 0)) |>
    dplyr::ungroup()

  # Warning for time jumps (missing data due to bluetooth glitch) would have occurred when sampling interval was calculated

  # 6. --- Optional Plotting --- ----
  # Line values at depths above the minimum stationary time
  if('obs_depth' %in% names(df_out)){
    stationary_depths <- unique(df_out$obs_depth[df_out$is_stationary_status == 999])
  } else {
    stationary_depths <-
      df_out |>
      dplyr::filter(is_stationary_status == 999) |>
      dplyr::group_by(stationary_block_id) |>
      dplyr::summarise(depth = round(mean({{depth_col}}, na.rm = TRUE), 1), .groups = "drop") |>
      dplyr::pull(depth)
  }
  # Plotting as before
  levels_list <- as.character(unique(df_out$is_stationary_status))
  # fixed colors
  mycolors <- c("0" = "red", "999" = "green")

  # intermediate durations (exclude 0 and 999)
  other_vals <- setdiff(levels_list, c("0", "999"))

  if(length(other_vals) > 0){
    # use Dark2 palette and interpolate if more colors are needed
    base_pal <- RColorBrewer::brewer.pal(8, "Dark2")              # base palette
    pal <- grDevices::colorRampPalette(base_pal)(length(other_vals))  # interpolate to needed length
    mycolors <- c(mycolors, setNames(pal, other_vals))
  }

  p1 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = {{depth_col}})) +
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2) +
    ggplot2::scale_y_reverse(breaks = seq(0,ceiling(max(depth_vals, na.rm = TRUE)))) +
    ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
    ggplot2::scale_color_manual(name = 'Seconds Stationary', values = mycolors) +
    ggplot2::geom_hline(yintercept = stationary_depths, col = 'black',lty = 2) +
    # ggplot2::theme_minimal()
    cowplot::theme_cowplot()

  p2 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = roll_range)) +
    ggplot2::geom_line(ggplot2::aes(color = "Rolling Range", linetype = "Rolling Range")) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = depth_range_threshold, color = "Threshold", linetype = "Threshold")) +
    ggplot2::scale_color_manual(name = "", values = c("Rolling Range" = "red", "Threshold" = "black")) +
    ggplot2::scale_linetype_manual(name = "", values = c("Rolling Range" = "solid", "Threshold" = "dashed")) +
    ggplot2::labs(title = "Rolling Range", y = "Range (m)", x = "Observation Index") +
    # ggplot2::theme_minimal()
    cowplot::theme_cowplot()

  print(patchwork::wrap_plots(p1, p2, ncol = 1))

  # 7. --- Compile output Data --- ----
  #  7a.*** Compute stationary depths --- ----
  df_out <- df_out |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(stationary_depth = mean(depth_m, na.rm = TRUE),
                  stationary_depth = ifelse(
                    is_stationary_status,
                    stationary_depth,
                    NA
                  ))

  #  7b.*** Remove unecessary columns --- ----

  if (drop_cols) {                                                                # Remove intermediate columns
    df_out <- df_out |>
      dplyr::select(-c(is_stationary_initial, stationary_block_id,block_n,block_secs))
  }


  return(df_out)

}


# data.frame(raw = dat_rename$depth_m,flag = is_stationary(dat_rename))
# junk <- is_stationary(df = dat_rename,
#               depth_range_threshold = 0.1,
#               start_trim_secs = 10,
#               stationary_secs = 45,
#               plot = TRUE)













