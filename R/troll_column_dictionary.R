#' Aqua TROLL Column Mapping and Metadata Dictionary
#'
#' A data dictionary used to map raw In-Situ Aqua TROLL sensor output headers
#' to standardized (canonical) variable names. It also contains metadata flags
#' used by internal functions to identify core parameters, instrument metadata,
#' and variables eligible for stability calculations.
#'
#' @format ## `troll_column_dictionary`
#' A tibble with `r nrow(troll_column_dictionary)` rows and 6 columns:
#' \describe{
#'   \item{pattern}{Character. Regex pattern used to match raw column headers
#'   from TROLL CSV/text exports.}
#'   \item{canonical}{Character. The standardized snake_case name used
#'   within the package.}
#'   \item{required}{Logical. If TRUE, these columns must be present for
#'   basic processing.}
#'   \item{meta}{Logical. Indicates if the column represents instrument
#'   diagnostics (e.g., battery, voltage) rather than environmental data.}
#'   \item{core_param}{Logical. Indicates if the column is a primary
#'   water quality parameter.}
#'   \item{stbl_calc}{Logical. Indicates if the parameter is eligible for
#'   sensor stability flagging in \code{TROLL_sensor_stable()}.}
#' }
#'
#' @seealso [TROLL_rename_cols()]
#' @examples
#' # View all core parameters
#' subset(troll_column_dictionary, core_param == TRUE)
"troll_column_dictionary"

troll_column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,  ~meta,  ~core_param,  ~stbl_calc,

  # Required core
  "^Date Time$",                              "DateTime",                TRUE,      FALSE,       FALSE,   FALSE,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,      FALSE,       FALSE,   FALSE,

  # Core parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,     FALSE,       TRUE,    TRUE,
  "^Temperature \\(\u00B0C\\)$",              "temperature_C",           FALSE,     FALSE,       TRUE,    TRUE,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,     FALSE,       TRUE,    TRUE,
  "^pH mV",                                   "pH_mV",                   FALSE,     FALSE,       FALSE,   FALSE,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,     FALSE,       TRUE,    TRUE,
  "^RDO Saturation",                          "DO_per",                  FALSE,     FALSE,       TRUE,   FALSE,
  "^Turbidity",                               "turbidity_NTU",           FALSE,     FALSE,       TRUE,    TRUE,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,     FALSE,       TRUE,    TRUE,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,     FALSE,       TRUE,    TRUE,
  "^ORP",                                     "ORP_mv",                  FALSE,     FALSE,       TRUE,    TRUE,
  "^Actual Conductivity \\(\u00B5S/cm\\)$",   "actual_conductivity_uScm",FALSE,     FALSE,       TRUE,    TRUE,

  # Additional Parameters
  "^Salinity \\(PSU\\)$",                     "salinity_psu",            FALSE,     FALSE,       FALSE,   FALSE,
  "^Resistivity \\(\u03A9\u22C5cm\\)$",       "resistivity_ohmcm",       FALSE,     FALSE,       FALSE,   FALSE,
  "^Density \\(g/cm\u00B3\\)$",               "density_gcm",             FALSE,     FALSE,       FALSE,   FALSE,
  "^Total Dissolved Solids \\(ppt\\)$",       "total_diss_solids_ppt",   FALSE,     FALSE,       FALSE,   FALSE,
  "^Oxygen Partial Pressure \\(Torr\\)$",     "oxy_part_press_torr",     FALSE,     FALSE,       FALSE,   FALSE,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,     FALSE,       FALSE,   FALSE,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,     TRUE,       FALSE,   FALSE,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,     TRUE,       FALSE,   FALSE,
  "^Pressure \\(psi\\)",                      "water_pressure_psi",      FALSE,     TRUE,       FALSE,   FALSE,
  "^Barometric Pressure \\(mm Hg\\)",         "barometric_pressure_mmHg",FALSE,     TRUE,       FALSE,   FALSE,
  "^Barometric Pressure \\(mbar\\)",          "barometric_pressure_mbar",FALSE,     TRUE,       FALSE,   FALSE,
  "^Latitude",                                "latitude_deg",            FALSE,     TRUE,       FALSE,   FALSE,
  "^Longitude",                               "longitude_deg",           FALSE,     TRUE,       FALSE,   FALSE,
  "^Marked$",                                 "marked_flag",             FALSE,     TRUE,       FALSE,   FALSE,
  "^Trollcom_temperature_C$",                 "Trollcom_temperature_C",  FALSE,     TRUE,       FALSE,   FALSE
)

# usethis::use_data(troll_column_dictionary,overwrite = TRUE)
