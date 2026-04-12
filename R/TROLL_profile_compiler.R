# Argument validation helper
validate_args <- function(
    stn_depthrange, stn_secs, stn_rollwindow_secs, stbl_settle_secs,
    stbl_min_secs,
    summarize_data, drop_cols, plot, stbl_range_thresholds,
    stability_ranges # pass your defaults table
) {

  # --- Numeric checks ---
  check_numeric(stn_depthrange, "stn_depthrange")
  check_numeric(stn_secs, "stn_secs")
  check_numeric(stn_rollwindow_secs, "stn_rollwindow_secs")
  check_numeric(stbl_settle_secs, "stbl_settle_secs", allow_zero = TRUE)
  check_numeric(stbl_min_secs, "stbl_min_secs")

  if (stbl_settle_secs >= stn_secs) stop("`stbl_settle_secs` must be less than `stn_secs`.")
  if (stn_rollwindow_secs > stn_secs) stop("`stn_rollwindow_secs` must be <= `stn_secs`.")

 if (stbl_min_secs > stn_secs)
    warning("`stbl_min_secs` > `stn_secs`; summaries may not be computed.")

  # --- Logical checks ---
  check_logical(summarize_data, "summarize_data")
  check_logical(drop_cols, "drop_cols")

  # --- Plot checks (single logical applies to both in normalization helper below) ---
  if (!is.logical(plot) || any(is.na(plot))) stop("`plot` must be logical.")
  if (!is.null(names(plot)) && !all(names(plot) %in% c("Final", "Stationary")))
    stop("`plot` names must be 'Final' and/or 'Stationary'.")

  # --- Range thresholds checks ---
  if (!is.null(stbl_range_thresholds)) {
    if (!is.numeric(stbl_range_thresholds))
      stop("`stbl_range_thresholds` must be a numeric named vector.")
    if (is.null(names(stbl_range_thresholds)) || any(names(stbl_range_thresholds) == ""))
      stop("`stbl_range_thresholds` must have non-empty names.")
    if (any(is.na(stbl_range_thresholds))) stop("`stbl_range_thresholds` cannot contain NA.")

    # Check for unknown params in stability_ranges
    unknown_params <- setdiff(names(stbl_range_thresholds), stability_ranges$param)

    if (length(unknown_params) > 0) {
      stop(
        paste0(
          "\nUnknown parameter(s) in `stbl_range_thresholds`:\n",
          paste0("  - ", unknown_params, collapse = "\n"),
          "\n\nThese do not match known parameters in `stability_ranges`.\n",
          "Check for typos and that the Unknown has values in 'stability_ranges'.\n",
          "If missing from `stability_ranges' submit an issue on GitHub."
        )
      )
    }
  }
}

# Argument normalization / updating helper
normalize_args <- function(plot,
                           stbl_range_thresholds,
                           stability_ranges) {

  # --- Normalize plotting vector ---
  def_plot <- c(Final = FALSE, Stationary = FALSE)

  # If single TRUE/FALSE, apply to both
  if (length(plot) == 1 && is.logical(plot)) {
    plot <- setNames(rep(plot, length(def_plot)), names(def_plot))
  }

  # If named vector, overwrite defaults
  if (!is.null(names(plot))) {
    def_plot[names(plot)] <- plot
    plot <- def_plot
  }

  # --- Normalize thresholds ---
  # Update default range thresholds with user provided input
  if (is.logical(stbl_range_thresholds)) {
    stop("\n`stbl_range_thresholds` must be numeric, not logical. Did you accidentally pass TRUE/FALSE?\n")
  }

  ranges <- stability_ranges
  if (!is.null(stbl_range_thresholds)) {
    update_tbl <- tibble::tibble(
      param = names(stbl_range_thresholds),
      range = unname(stbl_range_thresholds)
    ) |>
      dplyr::filter(param %in% ranges$param)
    if (nrow(update_tbl) > 0) ranges <- dplyr::rows_update(ranges, update_tbl, by = "param")
  }

  list(plot = plot, ranges = ranges)
}
#' Bulk Process TROLL Sonde Profiles
#'
#' \code{TROLL_profile_compiler()} reads raw TROLL sonde data, identifies stationary periods,
#' evaluates sensor stability for each parameter, optionally summarizes results,
#' and provides optional plotting of raw and final summarized data.
#'
#' @param path Character. Path to the raw TROLL CSV file.
#' @param depth_col Unquoted column name for water depth (numeric data).
#' @param datetime_col Unquoted column name for date-time (POSIXct, POSIXt, or Date).
#' @param stn_depthrange Numeric. Depth range threshold for \code{is_stationary()}.
#' @param stn_secs Numeric. Minimum time (seconds) required for a stationary block
#'   to be considered fully stationary (flagged by: \code{is_stationary_status = 999}).
#' @param stn_rollwindow_secs Numeric. Window size (seconds) used to compute rolling range for stationary detection.
#' @param stbl_settle_secs Numeric. Seconds to trim from the start of each stationary block.
#' @param stbl_min_secs Numeric. Minimum seconds used to calculate a summary
#'   statistic if stability is not reached within a stationary period.
#' @param stbl_range_thresholds Optional named numeric vector. Custom stability
#'   thresholds per parameter. See \code{\link{stability_ranges}} for defaults and naming.
#' @param summarize_data Logical. If \code{TRUE}, returns both `$Flagged_Data`
#'   and `$Summary_Data` (medians) as a list.
#' @param drop_cols Logical. If \code{TRUE}, removes intermediate processing columns.
#' @param plot Logical or named logical vector. Controls optional plotting from
#' @param debug Logical. If TRUE, prints the parameters being looped over during stability calculation to aid in debugging.
#' \code{is_stationary} and \code{TROLL_stable_summary}.
#' Default is \code{c(Final = FALSE,Stationary = FALSE)}. Single TRUE/FALSE applies to both.
#'
#' @return The output depends on the \code{summarize_data} argument:
#' \describe{
#'   \item{If \code{summarize_data = FALSE}}{Returns a \code{data.frame} with
#'   original sensor data and appended \code{_stable} logical flags.}
#'   \item{If \code{summarize_data = TRUE}}{Returns a \code{list} containing:
#'     \itemize{
#'       \item \code{Flagged_Data}: The full data frame with stability flags.
#'       \item \code{Summary_Data}: A summarized tibble of median sensor
#'       readings for each stable stationary period.
#'     }
#'   }
#' }
#'
#' @details
#' This function executes the TROLL processing workflow in the following order:
#' \enumerate{
#'   \item \bold{Ingestion & Cleanup:} Reads data via \code{TROLL_read_data()}
#'   and standardizes names via \code{TROLL_rename_cols()}.
#'   \item \bold{Stationary Detection:} Uses \code{is_stationary()} to identify
#'   depths where the sonde was held steady. Arguments for this step are
#'   prefixed with \code{stn_}.
#'   \item \bold{Stability Evaluation:} Calls \code{TROLL_sensor_stable()} for
#'   each parameter to identify "equilibrated" data points. Arguments for
#'   this step are prefixed with \code{stbl_}. Core parameters in data are
#'   automatically detected from: \code{troll_column_dictionary[troll_column_dictionary$stbl_calc==TRUE,]}.
#'   \item \bold{Summarization:} If requested, calculates median values for stable
#'   windows at each depth using \code{TROLL_stable_summary()}.
#'   \item \bold{Visualization:} Optionally generates diagnostic plots comparing raw
#'   profiles to filtered stationary/stable points.
#' }
#'
#' @examples
#' \dontrun{
#' # Standard profile compilation
#' result <- TROLL_profile_compiler(
#'   path = "extdata/sonde_file.csv",
#'   depth_col = depth_m,
#'   datetime_col = DateTime,
#'   stn_depthrange = 0.1,
#'   stn_secs = 45,
#'   stbl_min_secs = 5,
#'   plot = c(Final = TRUE, Stationary = FALSE)
#' )
#'
#' # Access results
#' head(result$Flagged_Data)
#' head(result$Summary_Data)
#' }
#'
#' @seealso \code{\link{TROLL_read_data}}, \code{\link{TROLL_rename_cols}},
#' \code{\link{is_stationary}}, \code{\link{TROLL_sensor_stable}},
#' \code{\link{TROLL_stable_summary}}
#'
#' @export
TROLL_profile_compiler <- function(path,                                         # Path to csv file
                                   depth_col = depth_m,                                    # Unquoted depth data column name
                                   datetime_col = DateTime,                                 # Unquoted datetime data column name
                                   # is_stationary
                                   stn_depthrange = 0.1,                         # Rolling range setting input to is_stationary()
                                   stn_secs = 45,                                # Time required after starttrim for is_stationary_status to be set to 999 (fully stationary)
                                   stn_rollwindow_secs = 10,                     # Size of the window used to calculate rolling range within is_stationary()
                                   # sensor_stable
                                   stbl_settle_secs = 10,                         # Number of seconds to be trimmed off the start of each stationary block
                                   stbl_min_secs = 5,                            # Minimum time to be used to calculate summary stat if stability is not detected within a stationary block
                                   stbl_range_thresholds = NULL,                 # Optionally provide custom range thresholds for individual params to detect sensor stability
                                   # Optional controls
                                   summarize_data = TRUE,                        # Optionally compile the final data as median of stationary & stable periods
                                   # check_target_depths = FALSE,                  # Option for user to check target depths against extracted stationary depths
                                   drop_cols = TRUE,                             # Optionally return internmediate column from is_stationary and TROLL_sensor_stable
                                   plot = c(Final = FALSE,Stationary = FALSE),    # Toggle optional plotting
                                   debug = FALSE
){


  # 0. --- Input validation / Checks --- ----
  # Tidy eval
  depth_col <- rlang::enquo(depth_col)
  depth_name <- rlang::as_name(depth_col)

  datetime_col <-  rlang::enquo(datetime_col)
  datetime_name <- rlang::as_name(datetime_col)
  # Validation helper
  validate_args(
    stn_depthrange, stn_secs, stn_rollwindow_secs, stbl_settle_secs,
    stbl_min_secs,
    summarize_data, drop_cols, plot, stbl_range_thresholds,
    stability_ranges
  )

  # 1. --- Update defaults w/ user inputs ----
  # Normalization helper updates using user inputs
  args_norm <- normalize_args(plot, stbl_range_thresholds, stability_ranges)

  # Updated ranges
  ranges <- args_norm$ranges

  # Optional plotting
  plot <- args_norm$plot

  # 2. --- Data Read --- ----
  dat_read <- TROLL_read_data(path = path)

  # 2. --- Rename Column and Clean unnecessary data --- ----
  dat_rename <- TROLL_rename_cols(df = dat_read,
                                  trollcomm_serials = trollCOMM_serials,
                                  strip_metadata = TRUE,
                                  verbose = FALSE)

  # Check data type of depth and datetime
  # if (!is.numeric(dat_rename[[rlang::as_string(depth_col)]])) {
  if (!(depth_name %in% names(dat_rename))) {
    stop("\n`depth_col` was not found in input data after renaming.\n")
  }
  if (!(datetime_name %in% names(dat_rename))) {
    stop("\n`datetime_col` was not found in input data after renaming.\n")
  }

  if (!is.numeric(dat_rename[[depth_name]])) {
    stop("\n`depth_col` must be numeric.\n")
  }

  if (!inherits(dat_rename[[datetime_name]], c("POSIXct", "POSIXt", "Date"))) {
    warning("`datetime_col` is not a recognized datetime format. Attempting coercion.")
  }
  # Check for presence of both DO mg/L and percent columns
  if(any(c('DO_mgL','DO_per') %in% names(dat_rename))){
    if(!'DO_per' %in% names(dat_rename)){
      warning('Dissolved oxygen concentration present but no percent data are included.')
    }
    if(!'DO_mgL' %in% names(dat_rename)){
      warning('Dissolved oxygen percent present, but no concentration data are included.')
    }
  }



  # 3. --- Identify parameter columns in data --- ----
  params <- names(dat_rename)[which(names(dat_rename) %in% troll_column_dictionary$canonical[troll_column_dictionary$stbl_calc])]

  if(length(params) <1 ){
    stop('\nNo sensor data columns identified. Column names must be standardized using the "TROLL_rename_cols" function.\n')
  }

  # Check for missing range thresholds for identified params (This should only happen if internal datasets get altered)
  missing_params <- setdiff(params, ranges$param)

  if (length(missing_params) > 0) {
    stop(
      paste0(
        "\nMissing range thresholds for the following parameters:\n",
        paste0("  - ", missing_params, collapse = "\n"),
        "\n\nAdd them to `stbl_range_thresholds` or your default `stability_ranges`."
      )
    )
  }

  # enforce uniqueness
  dup_params <- ranges$param[duplicated(ranges$param)]

  if (length(dup_params) > 0) {
    stop(
      paste0(
        "\nDuplicate parameter entries found in range thresholds:\n",
        paste0("  - ", unique(dup_params), collapse = "\n")
      )
    )
  }

  # 4. --- Detect when sonde is stationary --- ----
  dat_stationary <- is_stationary(df = dat_rename,
                                  depth_col = !!depth_col,
                                  datetime_col = !!datetime_col,
                                  depth_range_threshold = stn_depthrange,
                                  stationary_secs = stn_secs,
                                  rolling_range_secs = stn_rollwindow_secs,
                                  drop_cols = drop_cols,
                                  plot = plot["Stationary"])  # Control via named vector


  # Warn if no stationary depths found
  if(all(is.na(dat_stationary$stationary_depth))){
    stop('\nNo stationary periods detected. Check setting for "stn_" inputs.\n')
  }
  # x. Optionally match stationary depths to target depths ----
  # troll_run_stats()

  # 5. --- Compile sensor stability flags --- ----
  # Pre-allocate output
  out_list <- vector("list", length(params))
  names(out_list) <- paste0(params,'_stable')

  # Iterate over the unique parameters and check stability
  for (i in seq_along(params)) {
    if(debug){print(params[i])}
    # Extract each parameter
    param_i <- rlang::sym(params[i])

    # Create a holder for the stability flag
    flag_col <- paste0(params[i], "_stable")

    # Add the flag column only to output
    stable_i <- TROLL_sensor_stable(
      df = dat_stationary,
      settling_secs = stbl_settle_secs,
      value_col = !!param_i,
      min_median_secs = stbl_min_secs,
      range_thresh = ranges$range[ranges$param == params[i]]    # Set the rolling range threshold for individual params in the data (ranges object was updated if user input provided)
    )

    out_list[[i]] <- stable_i |>
      dplyr::pull(flag_col)
  }

  # Overwrite is_stationary_status to add settling trimming flag (888)
  dat_stationary$is_stationary_status <- stable_i$is_stationary_status

  # locate each flag column next to the sensor data column its associated with
  out <- dplyr::bind_cols(out_list)

  out <- dat_stationary |>
    dplyr::bind_cols(out)

  # Relocate each flag next to its sensor column
  for (p in params) {
    out <- out |>
      dplyr::relocate(
        dplyr::all_of(paste0(p, "_stable")),
        .after = dplyr::all_of(p)
      )
  }

  # 6. --- Summarize final results --- ----
  if(summarize_data){
    out_final <- list()

    # Add the flagged dataframe to output list
    out_final[['Flagged_Data']] <- out

    # Compile summarized dataframe using only stable data to calculate median
    stable_summary <- TROLL_stable_summary(df = out,
                                           group_col = stationary_depth,        # Bare column named OK, handled within function
                                           summary_fn = median)
    # Add the summarized data to the output list
    out_final[['Summary_Data']] <- stable_summary
  }

  # 8. Optionally plot the medians over raw data ----
  if(plot["Final"] & !summarize_data){
    stop('\nCannot plot summary data when `summarize_data` is FALSE.')
  }

  if(plot["Final"] && summarize_data){
    for (i in params) {

      # plot_df <- out|>
      #   dplyr::mutate(
      #     plot_status = dplyr::case_when(
      #       is_stationary_status == 999 ~ "Sonde Stationary",
      #       .data[[paste0(i, "_stable")]] %in% TRUE ~ "Sensor Stable",
      #       is_stationary_status == 888 ~ "Trimmed: settling",
      #       TRUE ~ "Sonde Moving"
      #     )
      #   )

      # plot_df <- out |>
      #   dplyr::mutate(
      #     stable_flag = .data[[paste0(i, "_stable")]] %in% TRUE,
      #     plot_status = dplyr::case_when(
      #       stable_flag ~ "Sensor Stable",
      #       is_stationary_status == 888 ~ "Trimmed: settling",
      #       is_stationary_status == 999 ~ "Sonde Stationary",
      #       TRUE ~ "Sonde Moving"
      #     ),
      #     plot_status = factor(
      #       plot_status,
      #       levels = c("Sonde Moving", "Sonde Stationary", "Trimmed: settling", "Sensor Stable")
      #     )
      #   )
      plot_df <- out |>
        dplyr::mutate(
          value = .data[[i]],
          stable_flag = .data[[paste0(i, "_stable")]] %in% TRUE,
          plot_status = dplyr::case_when(
            stable_flag ~ "Sensor Stable",
            is_stationary_status == 888 ~ "Trimmed: settling",
            is_stationary_status == 999 ~ "Sonde Stationary",
            TRUE ~ "Sonde Moving"
          ),
          plot_status = factor(
            plot_status,
            levels = c("Sonde Moving", "Sonde Stationary", "Trimmed: settling", "Sensor Stable")
          )
        )

      # return(plot_df)

      flag_data_column <- rlang::sym(i)
      depth_column_sym <- rlang::sym(depth_name)
      summary_column <- rlang::sym(paste0(i,'_median'))

      print(
      #   ggplot2::ggplot() +
      #     ggplot2::geom_path(data = plot_df,
      #                        ggplot2::aes(x = !!flag_data_column, y = !!depth_column_sym, color = 'All Data')) +
      #     ggplot2::geom_point(data = plot_df,
      #                         ggplot2::aes(x = !!flag_data_column, y = !!depth_column_sym, color = plot_status)) +
      #     # ggplot2::geom_point(data = out,
      #     #                     ggplot2::aes(x = !!flag_data_column, y = !!depth_column_sym, color = 'Raw Data')) +
      #     # ggplot2::geom_path(data = out,
      #     #                    ggplot2::aes(x = !!flag_data_column, y = !!depth_column_sym, color = 'Raw Data')) +
      #     ggplot2::geom_point(data = out |> dplyr::filter(is_stationary_status < 999),
      #                         ggplot2::aes(x = !!flag_data_column, y = !!depth_column_sym, color = 'Sonde Moving')) +
      #     ggplot2::geom_point(data = stable_summary,
      #                         ggplot2::aes(x = !!summary_column,y=stationary_depth, color = 'Final'),
      #                         pch = 17, cex = 3) +
      #     ggplot2::scale_y_reverse() +
      #     ggplot2::scale_x_continuous(position = 'top') +
      #     ggplot2::labs(y = 'Depth (m)') +
      #     ggplot2::scale_color_manual(name = '',
      #                                 # values = c('All Data' = 'grey80',
      #                                 #            'Sonde Moving' = 'firebrick1',
      #                                 #            'Sonde Stationary' = 'firebrick4',
      #                                 #            'Trimmed: settling' = 'goldenrod2',
      #                                 #            'Sensor Stable' = 'forestgreen',
      #                                 #            'Final' = 'dodgerblue')) +
      #                                 values = c('All Data' = 'grey80',
      #                                            'Sonde Moving' = 'firebrick1',
      #                                            'Sonde Stationary' = 'firebrick4',
      #                                            'Trimmed: settling' = 'goldenrod2',
      #                                            'Sensor Stable' = 'dodgerblue3',
      #                                            'Final' = 'dodgerblue')) +
      #                                 # breaks = c('All Data',
      #                                 #            'Sonde Moving',
      #                                 #            'Sonde Stationary',
      #                                 #            'Trimmed: settling',
      #                                 #            'Sensor Stable',
      #                                 #            'Final'),
      #                                 # drop = FALSE) +
      #     cowplot::theme_cowplot()
      # )
        ggplot2::ggplot(plot_df) +
          ggplot2::geom_path(
            ggplot2::aes(x = value, y = .data[[depth_name]], group = 1, color = "All Data")
          ) +
          ggplot2::geom_point(
            data = dplyr::filter(plot_df, is_stationary_status < 999),
            ggplot2::aes(x = value, y = .data[[depth_name]], color = "Sonde Moving")
          ) +
          ggplot2::geom_point(
            ggplot2::aes(x = value, y = .data[[depth_name]], color = plot_status)
          ) +
          ggplot2::geom_point(
            data = stable_summary,
            ggplot2::aes(x = .data[[paste0(i, "_median")]], y = stationary_depth, color = "Final"),
            shape = 17, size = 3
          ) +
          ggplot2::scale_y_reverse() +
          ggplot2::scale_x_continuous(position = "top") +
          ggplot2::labs(y = "Depth (m)",
                        x = i) +
          ggplot2::scale_color_manual(
            name = "",
            values = c(
              "All Data" = "grey80",
              "Sonde Moving" = "firebrick1",
              "Sonde Stationary" = "firebrick4",
              "Trimmed: settling" = "goldenrod2",
              "Sensor Stable" = "dodgerblue3",
              "Final" = "dodgerblue"
            )
          ) +
          cowplot::theme_cowplot()
      )
    }
  }
  # . --- Return Final Data --- ----
  if(!summarize_data) return(out)
  return(out_final)
}

