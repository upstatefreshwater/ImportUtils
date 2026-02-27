pH_stable <- function(df,
                      min_n = 5,               # minimum rows to keep per block
                      sampling_int = 2,        # seconds
                      slope_thresh = 0.01,     # units/minute
                      stationary_thresh = 998){

  # --- 1. Input checks ---
  req_cols <- c('DateTime','depth_m','obs_depth','stationary_block_id',
                'is_stationary_status','post_jiggle','pH_units')

  missingCols <- req_cols[!req_cols %in% names(df)]
  if(length(missingCols) > 0){
    stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
  }

  # Keep input safe
  data_in <- df

  # --- 2. Filter and split stationary post-jiggle groups ---
  stable_groups_split <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(DateTime, depth_m, obs_depth, stationary_block_id,
                  is_stationary_status, post_jiggle, pH_units) %>%
    dplyr::filter(post_jiggle & is_stationary_status > stationary_thresh & !is.na(pH_units)) %>%
    dplyr::group_by(stationary_block_id) %>%
    dplyr::mutate(t = as.numeric(difftime(DateTime, DateTime[1], units = "secs"))) %>%
    dplyr::group_split()

  # --- 3. Iterate blocks and compute slopes ---
  out <- tibble()

  for (block_idx in seq_along(stable_groups_split)) {

    block_df <- stable_groups_split[[block_idx]]
    n_rows <- nrow(block_df)

    group_out <- tibble(
      stationary_block_id = block_df$stationary_block_id,
      DateTime = block_df$DateTime,
      n_used = NA_real_,
      slope = NA_real_,
      n_dropped = NA_real_,
      iter_idx = 1:n_rows,
      meets_thresh = FALSE
    )

    dropped <- 0
    j <- 1

    # Compute slopes while truncating from the start
    while(nrow(block_df) >= min_n){
      fit <- lm(pH_units ~ t, data = block_df)
      slope_fit <- coef(fit)[["t"]]

      group_out$slope[j] <- slope_fit
      group_out$n_dropped[j] <- dropped
      group_out$n_used[j] <- nrow(block_df)

      if(abs(slope_fit) <= slope_thresh){
        group_out$meets_thresh[j] <- TRUE
      }

      block_df <- block_df[-1, ]
      dropped <- dropped + 1
      j <- j + 1
    }

    # --- 4. Determine rows to keep ---
    group_out <- na.omit(group_out)
    slopes <- group_out$slope
    never_met_thresh <- !any(abs(slopes) < slope_thresh)

    if(all(slopes >= 0) | all(slopes <= 0) | never_met_thresh){
      # keep at least min_n rows from the end
      keep_idx <- max(1, nrow(group_out) - min_n + 1):nrow(group_out)
      group_out <- group_out[keep_idx, ]
    } else {
      first_below_idx <- which(abs(group_out$slope) < slope_thresh)[1]
      remaining_rows <- nrow(group_out) - first_below_idx + 1
      if(remaining_rows < min_n){
        keep_idx <- max(1, nrow(group_out) - min_n + 1):nrow(group_out)
      } else{
        keep_idx <- first_below_idx:nrow(group_out)
      }
      group_out <- group_out[keep_idx, ]
    }

    # Append to master output
    out <- dplyr::bind_rows(out, group_out)
  }

  # --- 5. Join results back to full dataset ---
  final <- data_in %>%
    dplyr::left_join(out, by = c("DateTime", "stationary_block_id"))

  return(final)
}


### UNTESTED ###
pH_stable <- function(df,
                      min_n = 5,               # minimum rows to keep per block
                      sampling_int = 2,        # seconds
                      slope_thresh = 0.01,     # units/minute
                      stationary_thresh = 998){

  # --- 1. Input checks ---
  req_cols <- c('DateTime','depth_m','obs_depth','stationary_block_id',
                'is_stationary_status','post_jiggle','pH_units')

  missingCols <- req_cols[!req_cols %in% names(df)]
  if(length(missingCols) > 0){
    stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
  }

  # Keep input safe
  data_in <- df %>% dplyr::mutate(stable_pH_flag = FALSE)  # initialize flag

  # --- 2. Filter and split stationary post-jiggle groups ---
  stable_groups_split <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(DateTime, depth_m, obs_depth, stationary_block_id,
                  is_stationary_status, post_jiggle, pH_units) %>%
    dplyr::filter(post_jiggle & is_stationary_status > stationary_thresh & !is.na(pH_units)) %>%
    dplyr::group_by(stationary_block_id) %>%
    dplyr::mutate(t = as.numeric(difftime(DateTime, DateTime[1], units = "secs"))) %>%
    dplyr::group_split()

  # --- 3. Iterate blocks and compute slopes ---
  out <- tibble()

  for (block_idx in seq_along(stable_groups_split)) {

    block_df <- stable_groups_split[[block_idx]]
    n_rows <- nrow(block_df)

    group_out <- tibble(
      stationary_block_id = block_df$stationary_block_id,
      DateTime = block_df$DateTime,
      n_used = NA_real_,
      slope = NA_real_,
      n_dropped = NA_real_,
      iter_idx = 1:n_rows,
      meets_thresh = FALSE
    )

    dropped <- 0
    j <- 1

    # Compute slopes while truncating from the start
    while(nrow(block_df) >= min_n){
      fit <- lm(pH_units ~ t, data = block_df)
      slope_fit <- coef(fit)[["t"]]

      group_out$slope[j] <- slope_fit
      group_out$n_dropped[j] <- dropped
      group_out$n_used[j] <- nrow(block_df)

      if(abs(slope_fit) <= slope_thresh){
        group_out$meets_thresh[j] <- TRUE
      }

      block_df <- block_df[-1, ]
      dropped <- dropped + 1
      j <- j + 1
    }

    # --- 4. Determine rows to keep ---
    group_out <- na.omit(group_out)
    slopes <- group_out$slope
    never_met_thresh <- !any(abs(slopes) < slope_thresh)

    if(all(slopes >= 0) | all(slopes <= 0) | never_met_thresh){
      keep_idx <- max(1, nrow(group_out) - min_n + 1):nrow(group_out)
      group_out <- group_out[keep_idx, ]
    } else {
      first_below_idx <- which(abs(group_out$slope) < slope_thresh)[1]
      remaining_rows <- nrow(group_out) - first_below_idx + 1
      if(remaining_rows < min_n){
        keep_idx <- max(1, nrow(group_out) - min_n + 1):nrow(group_out)
      } else{
        keep_idx <- first_below_idx:nrow(group_out)
      }
      group_out <- group_out[keep_idx, ]
    }

    # Append to master output
    out <- dplyr::bind_rows(out, group_out %>% dplyr::select(DateTime, stationary_block_id))
  }

  # --- 5. Update stable_pH_flag in original dataset ---
  final <- data_in %>%
    dplyr::left_join(
      out %>% dplyr::mutate(stable_pH_flag = TRUE),
      by = c("DateTime","stationary_block_id")
    ) %>%
    dplyr::mutate(stable_pH_flag = ifelse(is.na(stable_pH_flag.y), stable_pH_flag.x, stable_pH_flag.y)) %>%
    dplyr::select(-stable_pH_flag.x, -stable_pH_flag.y)

  return(final)
}
