depth_rounder <- function(df,
                          depth_col = depth_m,
                          # depth_interval = 1,
                          target_depths = list(interval = 1,
                                               values = NULL),
                          tolerance = 0.2) {

  # Validation: Tolerance
  if (!is.numeric(tolerance) || tolerance < 0) {
    stop("`tolerance` must be a non-negative numeric.")
  }

  # Stop if tolerance is too precise
  if (decimalplaces(tolerance) > 2) {
    stop(paste0("Depth tolerance value: ", tolerance, " is unrealistically small."))
  }

  # --- Pull tolerance precision ----
  tol_precision <- decimalplaces(tolerance)
  # Update target depths via user input
  default_targ_depths <- list(interval = 1,
                              values = NULL)
  updated_targ_depths <- utils::modifyList(default_targ_depths,target_depths) # Second input arg. overwrite first


  # Validation: Check the 'target_depths' provided by the user specifically
  has_interval <- !is.null(target_depths$interval)
  has_vals     <- !is.null(target_depths$values)

  # Don't allow specification of both input to target_depths
  if (has_interval && has_vals) {
    stop("Specify either 'interval' OR 'values', not both.\nUse ex: 'interval = 1' for regular increments or 'values = c(...)' for irregular depths.")
  }

  # Type Checking of target_depths inputs
  if (!is.numeric(updated_targ_depths$interval) || length(updated_targ_depths$interval) != 1) {
    # If they provided values, we don't care about the default interval
    if (!has_vals) stop("'interval' must be a single numeric value.")
  }

  if (has_vals && !is.numeric(updated_targ_depths$values)) {
    stop("'values' must be a numeric vector.")
  }

  if (has_vals && length(updated_targ_depths$values) == 1) {
    warning("Only a single target depth value provided.")
  }

  # Extract the target for depth rounding
  targ_depth_type <- if(names(updated_targ_depths) == 'interval') 'interval' else 'values'
  interval <- updated_targ_depths[targ_depth_type]

  # --- Branch depending on which type of depths are given ---
  if(targ_depth_type == 'interval') {
    data_out <- df |>
      dplyr::mutate(
        orig_depth = round({{depth_col}}, tol_precision),                        # Only keep precision to tolerance level
        nearest_depth_interval = round(orig_depth / depth_interval) * depth_interval,        # This checks for closest given depth_interval, allowing decimal depth_intervals too
        # Round the difference to 6 decimals to avoid floating-point issues
        flag_depth = dplyr::case_when(
          orig_depth < 0.5 ~ '',
          abs(orig_depth - nearest_depth_interval) > tolerance + .Machine$double.eps^0.5 ~ 'flag', # This does the work
          TRUE ~ ''
        ),
        obs_depth = round(orig_depth / depth_interval) * depth_interval
      ) |>
      dplyr::select(-nearest_depth_interval)
  } else{
    data_out <- df |>
      dplyr::mutate(
        orig_depth = round({{depth_col}}, tol_precision),                        # Only keep precision to tolerance level
        nearest_depth_interval = round(orig_depth / depth_interval) * depth_interval,        # This checks for closest given depth_interval, allowing decimal depth_intervals too
        # Round the difference to 6 decimals to avoid floating-point issues
        flag_depth = dplyr::case_when(
          orig_depth < 0.5 ~ '',
          abs(orig_depth - nearest_depth_interval) > tolerance + .Machine$double.eps^0.5 ~ 'flag', # This does the work
          TRUE ~ ''
        ),
        obs_depth = round(orig_depth / depth_interval) * depth_interval
      ) |>
      dplyr::select(-nearest_depth_interval)
  }
}


  if (!is.numeric(depth_interval) || depth_interval <= 0) {# Checks depth_interval is positive, or NULL
    stop("depth_interval must be positive numeric, or NULL")}

  # If regular depth_intervals enter this loop
  if(!is.null(depth_interval)){
    int_dec <- decimalplaces(depth_interval)
    if(int_dec>2) stop(paste0('Depth depth_interval: ',depth_interval,' is unrealistically small.'))

    data_out <- df |>
      dplyr::mutate(
        orig_depth = round({{depth_col}}, tol_precision),                        # Only keep precision to tolerance level
        nearest_depth_interval = round(orig_depth / depth_interval) * depth_interval,        # This checks for closest given depth_interval, allowing decimal depth_intervals too
        # Round the difference to 6 decimals to avoid floating-point issues
        flag_depth = dplyr::case_when(
          orig_depth < 0.5 ~ '',
          abs(orig_depth - nearest_depth_interval) > tolerance + .Machine$double.eps^0.5 ~ 'flag', # This does the work
          TRUE ~ ''
        ),
        obs_depth = round(orig_depth / depth_interval) * depth_interval
      ) |>
      dplyr::select(-nearest_depth_interval)
  }

  if(is.null(depth_interval)){
    stop('Irregular depth_interval support not implemented \nIf you meant to use regular depth depth_intervals, check your "target_depths" specification.')
  }


  return(data_out)
}
