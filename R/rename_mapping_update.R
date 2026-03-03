column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,

  # Required core
  "^Date Time$",                             "DateTime",                TRUE,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,

  # Core parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,
  "^Temperature \\(°C\\)$",                   "temperature_C",           FALSE,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,
  "^pH mV",                                   "pH_mV",                   FALSE,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,
  "^RDO Saturation",                          "DO_per",                  FALSE,
  "^Turbidity",                               "turbidity_NTU",           FALSE,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,
  "^Barometric Pressure",                     "barometric_pressure_mbar",FALSE,
  "^Latitude",                                "latitude_deg",            FALSE,
  "^Longitude",                               "longitude_deg",           FALSE,
  "^Marked$",                                 "marked_flag",             FALSE,
  "^Trollcom_temperature_C$",                     "Trollcom_temperature_C",  FALSE
)

# column_dictionary

apply_schema <- function(data) {

  # For each column in the data, find a regex pattern from the dictionary, then replace it with the canonical version
  rename_map <- purrr::map_chr(names(data), function(col) {

    matches <- column_dictionary[
      stringr::str_detect(col, column_dictionary$pattern),
    ]

    if (nrow(matches) == 1) {
      matches$canonical
    } else {
      NA_character_
    }
  })

  # Check for duplicated column names after renaming
  dup_canonical <- rename_map[duplicated(rename_map)]

  if (length(dup_canonical)) {
    stop(
      "Multiple columns matched the same canonical name:\n",
      paste(unique(dup_canonical), collapse = "\n"),
      "\n\nCannot safely proceed."
    )
  }

  # Check for unknown column names
  unknown_cols <- names(data)[is.na(rename_map)]

  if (length(unknown_cols)) {
    stop(
      "Unknown column(s) detected:\n",
      paste(unknown_cols, collapse = "\n"),
      "\n\nUpdate column_dictionary if these are valid new sensors."
    )
  }

  names(data) <- rename_map

  # ---- Required columns check ----
  required <- column_dictionary$canonical[column_dictionary$required]

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

normalize_raw_names <- function(data) {
  new_names <- names(data) |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s*\\(\\d+\\)", "")  # remove serial numbers

  names(data) <- new_names
  data
}

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

      warning(
        "Multiple TROLL-COM serials detected: ",
        paste(comm_sn, collapse = ", "),
        ".\nCannot automatically select which temperature column to use."
      )

    }
    return(data)
  }
}
one <- detect_trollcom(data = dat,trollCOMM_serials = trollCOMM_serials)
two <- normalize_raw_names(one)
three <- apply_schema(two)
