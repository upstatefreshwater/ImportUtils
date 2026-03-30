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

