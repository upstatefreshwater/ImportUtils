source('R/TROLL_read_data.R')
source('R/TROLL_rename_cols.R')
source('R/is_stationary.R')
# source('R/final_sensor_stable.R')
source('R/utils.R')


# 0. Required Inputs ----

# Data read and column names
path = 'inst/extdata/2025-09-16_LT1.csv'
depth_col = depth_m
datetime_col = DateTime

# is_stationary
stn_depthrange = 0.1
stn_secs = 45
stn_rollwindow_secs = 10
stn_startrim_secs = 4

# sensor_stable
min_final_secs = 5
range_thresholds = NULL
stationary_thresh_secs = 10

# Optional controls
check_target_depths = FALSE
drop_cols = TRUE
plot = TRUE

# 0. Checks ----
if(!is.null(range_thresholds)){
  badnames <- !names(range_thresholds) %in% stability_ranges$param

  if(any(badnames)){
    badnames <- names(range_thresholds)[badnames]
    valid_names <- paste(stability_ranges$param, collapse = ", ")

    stop(paste0(
      "Incorrect names provided to 'range_thresholds':\n  ",
      paste(badnames, collapse = ", "),
      "\n\nMust be one of: \n  ",
      valid_names
    ), call. = FALSE)
  }
}
# 1. Read Data ----
dat_read <- TROLL_read_data(path = path)
# 2. Rename Column and Clean unnecessary data ----
dat_rename <- TROLL_rename_cols(df = dat_read,
                                trollcomm_serials = trollCOMM_serials,
                                strip_metadata = TRUE,
                                print_colnames = FALSE)

# 3. Identify parameter columns in data ----
params <- names(dat_rename)[which(names(dat_rename) %in% troll_column_dictionary$canonical[troll_column_dictionary$core_param])]

if(length(params) <1 ){
  stop('No sensor data columns identified. Column names must be standardized using the "TROLL_rename_cols" function.')
}
# 4. Detect when sonde is stationary ----
dat_stationary <- is_stationary(df = dat_rename,
                                depth_col = depth_m,
                                datetime_col = DateTime,
                                depth_range_threshold = 0.1,
                                stationary_secs = 35,
                                rolling_range_secs = 10,
                                start_trim_secs = 15,
                                drop_cols = TRUE,
                                plot = TRUE)
# 5. Optionally match stationary depths to target depths ----
# troll_run_stats()



# 6. Iteratively check sensor stability ----

# Update default range thresholds with user provided input
ranges <- stability_ranges

if(!is.null(range_thresholds)){

  ranges <- ranges |>
    dplyr::rows_update(
      tibble::tibble(
        param = names(range_thresholds),
        range_thresh = unname(range_thresholds)
      ),
      by = "param"
    )

}

out_list <- vector("list", length(params))
names(out_list) <- paste0(params,'_stable')



for (i in seq_along(params)) {
  param_i <- rlang::sym(params[i])
  # stop(print(param_i))
  # print(param_i)

  flag_col <- paste0(param_i, "_stable")

  out_list[[i]] <- TROLL_sensor_stable(
    df = dat_stationary,
    value_col = !!param_i,
    min_secs = min_final_secs,
    range_thresh = ranges$range_thresh[ranges$param == param],
    stationary_thresh = stationary_thresh_secs
  ) |>
    dplyr::pull(flag_col)


}
out <- dplyr::bind_cols(out_list)

junk <- dat_stationary |>
 dplyr::bind_cols(out) |>
  dplyr::relocate(sp_conductivity_uScm_stable,.after = sp_conductivity_uScm)

dat_stable <- TROLL_sensor_stable(dat_stationary,
                                  value_col = DO_mgL,
                                  min_secs = 5,
                                  slope_thresh = 0.05,
                                  range_thresh = 0.15,
                                  stationary_thresh = 998,
                                  drop_cols = FALSE)



