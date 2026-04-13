#' Summarize stable TROLL observations
#'
#' Summarizes sensor values using only observations flagged as stable
#' (i.e., where corresponding `<sensor>_stable` columns are `TRUE`).
#'
#' Intended to create final values from the \code{TROLL_profile_compiler}
#' function.
#'
#' The function automatically detects data columns paired with
#' `_stable` flag columns and computes a summary statistic for
#' each parameter within the specified grouping variable.
#'
#' @param df A data frame containing sensor observations and
#'   corresponding `_stable` flag columns.
#' @param group_col Column used to group observations (e.g.,
#'   `stationary_depth`).
#' @param summary_fn Summary function applied to stable values.
#'   Default is `median`.
#' @param digits Numeric. Number of decimal places to round result of `summary_fn`.
#'
#' @return A data frame with one row per group and summarized
#'   sensor values for each parameter.
#'
#' @importFrom stats median setNames
#'
#' @export
# TROLL_stable_summary <- function(df,
#                                  group_col = stationary_depth,
#                                  summary_fn = median) {
#
#   group_col <- rlang::ensym(group_col)
#
#   # Warn if range is given as summary function
#   if((identical(summary_fn, range))){
#     summary_fn = function(x,na.rm = TRUE) diff(range(x, na.rm = na.rm))
#     warning('Range given as summary function. Output will be returned the numerical range instead of min/max.')
#   }
#
#   # Identify stable flag columns
#   stable_cols <- names(df)[grepl("_stable$", names(df))]
#
#   # Derive paired value columns
#   value_cols <- sub("_stable$", "", stable_cols)
#
#   df |>
#     dplyr::ungroup() |>
#     dplyr::filter(is_stationary_status == 999) |> # Commented out because this is a redundant filter
#     dplyr::group_by(!!group_col) |>
#     dplyr::summarise(
#       dplyr::across(
#         dplyr::all_of(value_cols),
#         ~{
#           flag_col <- paste0(dplyr::cur_column(), "_stable")
#           flag_idx <- dplyr::cur_data()[[flag_col]]
#           vals <- .x[flag_idx]
#           if (length(vals) == 0) NA_real_ else summary_fn(vals, na.rm = TRUE)
#         },
#         .names = "{.col}_median"
#       ),
#       .groups = "drop"
#     )
# }
TROLL_stable_summary <- function(df,
                                 group_col = stationary_depth,
                                 summary_fn = median,
                                 digits = 4) {

  group_col <- rlang::ensym(group_col)

  # Warn if range is given as summary function
  if((identical(summary_fn, range))){
    summary_fn = function(x,na.rm = TRUE) diff(range(x, na.rm = na.rm))
    warning('Range given as summary function. Output will be returned the numerical range instead of min/max.')
  }

  stable_cols <- names(df)[grepl("_stable$", names(df))]
  value_cols <- sub("_stable$", "", stable_cols)

  df |>
    dplyr::ungroup() |>
    dplyr::filter(is_stationary_status == 999) |>
    dplyr::group_by(!!group_col) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_cols),
        ~{
          flag_col <- paste0(dplyr::cur_column(), "_stable")
          flag_idx <- dplyr::cur_data()[[flag_col]]
          vals <- .x[flag_idx]

          if (length(vals) == 0) {
            NA_real_
          } else {
            out <- summary_fn(vals, na.rm = TRUE)
            if (!is.null(sig_digits)) round(out, digits) else out
          }
        },
        .names = "{.col}_median"
      ),
      .groups = "drop"
    )
}
