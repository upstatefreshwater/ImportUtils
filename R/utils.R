# Finds the nearest depth from a raw data column to a candidate set
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
