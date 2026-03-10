# Finds the nearest depth from a raw data column to a candidate set of target depths
nearest_depth <- function(raw_z,
                          candidate_z,
                          tol = 0.25,
                          surface_tol = tol){

  # Compute differences between raw depths and candidates as absolute distances
  d <- abs(outer(raw_z, candidate_z, "-"))
  # Locate the index of which distance in smallest
  idx <- apply(d, 1, which.min)
  # Pull out the depths
  out <- candidate_z[idx]

  min_d <- apply(d, 1, min)

  out[min_d > tol] <- NA

  # special handling for 0 depth
  if(any(candidate_z == 0)){
    surf <- which(candidate_z == 0)
    surf_diff <- raw_z - candidate_z[surf]
    out[surf_diff >= 0 & surf_diff <= surface_tol] <- 0
  }

  out
}

# Calculates the sampling interval from a column/vector of datetime data as the mode (if multiple intervals detected)
# If multiple intervals detected, it gives a warning only

get_sample_interval <- function(datetime_data,
                                output_units = "secs",
                                tol_prop = 1) {

  # Calculate sampling intervals
  all_ints <- as.numeric(diff(datetime_data), units = "secs")
  all_ints <- all_ints[!is.na(all_ints)]

  # Count intervals
  counts <- table(all_ints)

  # Most common interval (mode)
  sampling_int <- as.numeric(names(counts)[which.max(counts)])

  # Check consistency
  prop <- max(counts) / sum(counts)

  if (length(counts) > 1 && prop < tol_prop) {

    dist_table <- data.frame(
      sampling_interval = as.numeric(names(counts)),
      n = as.integer(counts)
    )

    warning(
      paste0(
        "Inconsistent sampling intervals detected.\n",
        "Dominant interval: ", sampling_int, " ", output_units,
        " (", round(prop * 100, 1), "% of records).\n",
        "Interval distribution:\n",
        paste(capture.output(print(dist_table, row.names = FALSE)),
              collapse = "\n")
      ),
      call. = FALSE
    )
  }

  sampling_int
}

