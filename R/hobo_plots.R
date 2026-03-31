# ============================================
# HOBO Logger Analysis Pipeline
# ============================================

#' Plot HOBO Logger Data
#'
#' Creates time series plots of absolute pressure and temperature
#' from one or more HOBO logger datasets. If multiple datasets are provided,
#' they are overlaid and colored by dataset name.
#'
#' @param df A data frame containing at least the following columns:
#' \describe{
#'   \item{datetime}{POSIXct timestamp}
#'   \item{abspress}{Absolute pressure values}
#'   \item{temp}{Temperature values}
#'   \item{dataset}{Dataset/site name}
#' }
#'
#' @return A ggplot object showing faceted time series of pressure and temperature.
#'
#' @examples
#' \dontrun{
#' combined <- combine_hobos(Ischua_2, Ischua_3)
#' plot_hobos(combined)
#' }
#'
#' @export
plot_hobos <- function(df) {
  df_long <- tidyr::pivot_longer(df,
                                 cols = c(abspress, temp),
                                 names_to = "variable",
                                 values_to = "value")

  ggplot2::ggplot(df_long, ggplot2::aes(x = datetime, y = value, color = dataset)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "HOBO Logger Data",
      x = "Time",
      y = "Value",
      color = "Site"
    )
}

#' Combine HOBO Logger Datasets
#'
#' Combines multiple processed HOBO datasets into a single data frame.
#' Each dataset must already be processed using `formatischua()` and
#' include a `dataset` column.
#'
#' @param ... Data frames to combine.
#'
#' @return A single data frame containing all input datasets stacked together.
#'
#' @examples
#' \dontrun{
#' combined <- combine_hobos(Ischua_2, Ischua_3)
#' }
#'
#' @export
combine_hobos <- function(...) {
  dplyr::bind_rows(...)
}

# ============================================
# Example workflow
# ============================================

# 1. Process raw logger files
#Ischua_2 <- hoboformat("C:/Users/Annie/OneDrive/Desktop/Hobos/Ischua_2.csv", "Ischua_2")
#Ischua_3 <- hoboformat("C:/Users/Annie/OneDrive/Desktop/Hobos/Ischua_3.csv", "Ischua_3")

# 2. Combine datasets
#combined <- combine_hobos(Ischua_2, Ischua_3)

# 3. Plot
#plot_hobos(combined)


