
df = dat_stationary
value_col = 'DO_mgL'
sampling_int = 2
# range_window_secs = 10
# range_thresh = 0.1
# slope_thresh = 0.1
min_n = 5
stationary_thresh = 998                                                          # Setting this allows the user to decrease stationary time if necessary to meet min_n
jiggle_present = TRUE

n_range_window = 5

# 1. Input Checks ----
req_cols <- c('DateTime','obs_depth','stationary_block_id','is_stationary_status',value_col) # 'depth_m',

if(!all(req_cols %in% names(df))){
  missingCols <- req_cols[!req_cols %in% names(df)]
  stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
}

jiggle_check <- 'post_jiggle' %in% names(df)
if(!jiggle_present & jiggle_check){
  warning('You have elected to inlcude "jiggle period" data for stability determination, even though "post_jiggle" flags exist in the input data.')
}



# 2. Data wrangling ----
if(jiggle_present & jiggle_check){
  # Data if jiggle period is flagged
  dat_grouped <- df |>
    dplyr::filter(post_jiggle,
                  is_stationary_status > stationary_thresh,
                  !is.na(value_col)) |> #{{value_col}}
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")),
      roll_range = zoo::rollapplyr(
        value_col,
        width = n_range_window,
        FUN = function(x) if(all(!is.na(x))) max(x) - min(x) else NA,
        fill = NA
      )
    ) |>
    dplyr::group_split()
} else{
  dat_grouped <- df |>
    # dplyr::filter(is_stationary_status > stationary_thresh,
    #               !is.na(value_col)) |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")),
      roll_range = zoo::rollapplyr(
        value_col,
        width = n_range_window,
        FUN = function(x) if(all(!is.na(x))) max(x) - min(x) else NA,
        fill = NA
      )
    ) |>
    dplyr::group_split()
}

# 3.
p1 <- ggplot2::ggplot(data = dat_rnddepth |>
                        dplyr::mutate(depthdiff = c(NA,diff(depth_m))),
                      ggplot2::aes(x = DateTime,y=depth_m*-1)) +
  ggplot2::geom_point()

p2 <- ggplot2::ggplot(data = dat_rnddepth |> dplyr::mutate(depthdiff = abs(c(NA,diff(depth_m)))),
                      ggplot2::aes(x = DateTime,y=depthdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = seq(0,1,0.1),col='red',lty=2)
p1/p2
