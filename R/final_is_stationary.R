
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          start_trim_secs = 15,           # this becomes a combo of the rolling window and trimming at start of stationary block
                          stationary_secs = 60,
                          # sampling_int = 0,
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

  # 1. ---
}












if(samp_int > 30){
  # Would be good to automate a depth check in here
  message("Sampling interval > 30s detected. Sonde assumed fixed in position.")
  return(df |> dplyr::mutate(is_stationary_status = 999))
}
