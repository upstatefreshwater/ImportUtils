#' Apply TROLL Column Naming Schema
#'
#' Matches raw column names against the internal
#' \code{troll_column_dictionary} using regular expression patterns,
#' renames columns to their canonical internal names, and enforces
#' required column presence.
#'
#' This function will:
#' \itemize{
#'   \item Stop if a column does not match any schema dictonary pattern.
#'   \item Stop if multiple columns map to the same canonical name.
#'   \item Stop if required columns are missing.
#' }
#'
#' @param data A data frame containing raw or partially normalized
#'   TROLL data.
#' @param dictionary A tibble containing schema definitions with
#'   columns \code{pattern}, \code{canonical}, and \code{required}.
#'   Defaults to \code{troll_column_dictionary}.
#'
#' @return A data frame with validated and renamed canonical columns.
#'
#' @keywords internal

apply_trollname_schema <- function(data,
                                   dictionary = troll_column_dictionary) {

  # For each column in the data, find a regex pattern from the dictionary, create a "map" for renaming columns (just a vector of canonical names)
  rename_map <- purrr::map_chr(names(data), function(col) {

    matches <- troll_column_dictionary[
      stringr::str_detect(col, troll_column_dictionary$pattern),
    ]

    if (nrow(matches) == 1) {
      matches$canonical
    } else {
      NA_character_ # Note** This will force an "unkown column" error later
    }
  })

  # Check for duplicated column names within the rename map to be applied as the new column names (error)
  dup_canonical <- rename_map[duplicated(rename_map)]

  if (length(dup_canonical)) {
    stop(
      "Multiple columns matched the same canonical name:\n",
      paste(unique(dup_canonical), collapse = "\n"),
      "\n\nCannot safely proceed."
    )
  }

  # Check for unknown column names (error)
  unknown_cols <- names(data)[is.na(rename_map)]

  if (length(unknown_cols)) {
    stop(
      "Unknown column(s) detected:\n",
      paste(unknown_cols, collapse = "\n"),
      "\n\nUpdate troll_column_dictionary if these are valid new sensors."
    )
  }

  names(data) <- rename_map

  # ---- Required columns check ----
  required <- troll_column_dictionary$canonical[troll_column_dictionary$required]

  missing <- setdiff(required, names(data))

  if (length(missing)) {
    stop(
      "Missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  message("Schema validation passed: ",
          ncol(data), " columns recognized.")

  data
}

#' Normalize Raw Column Names
#'
#' Cleans raw TROLL column names by trimming whitespace and
#' removing serial numbers of the form \code{(1234567)}.
#'
#' This prepares column names for schema validation.
#'
#' @param data A data frame with raw column names.
#'
#' @return A data frame with normalized column names.
#'
#' @keywords internal
normalize_raw_names <- function(data) {
  new_names <- names(data) |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s*\\(\\d+\\)", "")  # remove serial numbers

  names(data) <- new_names
  data
}

#' Detect and Rename TROLL-COM Temperature Column
#'
#' Identifies TROLL-COM serial numbers in raw column names and,
#' when a single matching serial is detected, renames the associated
#' internal temperature column to \code{Trollcom_temperature_C}
#' to avoid collision with water temperature sensors.
#'
#' If multiple TROLL-COM serial numbers are detected, a warning
#' is issued and no automatic renaming is performed.
#'
#' @param data A data frame containing raw TROLL column names.
#' @param trollCOMM_serials A character vector of known TROLL-COM
#'   serial numbers used for detection.
#'
#' @return A data frame with TROLL-COM temperature renamed if applicable.
#'
#' @keywords internal
detect_trollcom <- function(data,
                            trollCOMM_serials) {

  # Extract columns that rely on the TROLL-comm (Baro press, internal temp)
  comm_cols <- names(data)[stringr::str_detect(
    names(data),
    paste(trollCOMM_serials, collapse = "|")
  )]

  # If no TROLL-comm data columns, just present a message to the console
  if (length(comm_cols) == 0) {

    message("No TROLL-COM data columns detected.")
    return(data)
    # If there are data columns check that there is only a single s/n
  } else {
    # Extract the serial numbers in the data based on the known TROLL-comm serials
    comm_sn <- stringr::str_extract(
      comm_cols,
      paste(trollCOMM_serials, collapse = "|")
    )

    # pull the serial(s)
    comm_sn <- unique(comm_sn[!is.na(comm_sn)])

    # If only one serial detected, rename the temp to avoid conflicts with water temp
    if (length(comm_sn) == 1) {

      comm_tempcol <- paste0("Temperature (°C) (", comm_sn, ")") # Need to apply the correct s/n in column name

      if (comm_tempcol %in% names(data)) {

        message(
          "TROLL-COM temperature detected (serial ", comm_sn, ").\n",
          "Renaming column: ", comm_tempcol
        )

        data <- data |>
          dplyr::rename(
            Trollcom_temperature_C = !!rlang::sym(comm_tempcol) # Needs tidy-eval selection here
          )

      }

    } else {

      stop(
        "Multiple TROLL-COM serials detected: ",
        paste(comm_sn, collapse = ", "),
        ".\nCannot automatically select which temperature column to use."
      )

    }
    return(data)
  }
}

#' Standardize TROLL Data Column Names
#'
#' Cleans, validates, and standardizes raw TROLL CSV column names
#' into names used throughout the package.
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects and renames TROLL-COM temperature columns.
#'   \item Normalizes raw column names (removes serial numbers).
#'   \item Applies the internal column names dictionary for validation
#'         and canonical renaming.
#' }
#'
#' The function will stop execution if:
#' \itemize{
#'   \item Unknown columns are detected,
#'   \item Multiple columns map to the same canonical name,
#'   \item Required columns are missing.
#' }
#'
#' @param df A data frame read from a raw TROLL CSV export.
#' @param trollcomm_serialnums A character vector of known TROLL-COM
#'   serial numbers. Defaults to \code{trollCOMM_serials}.
#' @param print_colnames Logical; if \code{TRUE}, prints the final
#'   standardized column names.
#'
#' @return A data frame with validated and standardized column names.
#'
#' @export

rename_trollcols <- function(df,
                             trollcomm_serialnums = trollCOMM_serials,
                             print_colnames =FALSE){

  one <-   detect_trollcom(data = df,trollCOMM_serials = trollCOMM_serials)
  two <-   normalize_raw_names(one)
  three <- apply_trollname_schema(two)

  if(print_colnames){
    column_list <- paste(colnames(three))
    message("The CSV has Columns:\n", paste(column_list, collapse = "\n"))
  }

  return(three)
}

