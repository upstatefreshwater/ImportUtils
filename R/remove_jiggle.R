remove_jiggle <- function(df,
                          sampling_int,
                          jiggle_secs = 15,
                          mode = 'flag',
                          stationary_flag_col = is_stationary_status){

  # Check stationary column existence safely
  stationary_name <- rlang::as_name(rlang::ensym(stationary_flag_col))

  if (!stationary_name %in% names(df)) {
    warning(
      "No stationary flag column in input dataframe."
    )
  }

  n_jiggle <- ceiling(jiggle_secs / sampling_int)                          # number of observations to throw out for "jiggle period"

  jiggle_dat <- df |>
    dplyr::mutate(
      stationary_block_id = dplyr::consecutive_id({{ stationary_flag_col }})
    ) |>
  # Group into stationary blocks
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      stationary_block_index = dplyr::row_number(),                    # integer index restarts on each stable block because of data grouping
      post_jiggle = stationary_block_index > n_jiggle,                 # flag the "jiggle_secs" aka the period after sonde stops moving when the field tech is supposed to slightly move sonde
    ) |>
    dplyr::ungroup()

  return(jiggle_dat)

}
