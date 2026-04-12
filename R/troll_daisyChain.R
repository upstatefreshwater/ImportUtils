# path <-
#   # 'inst/extdata/2025-05-13_LW1.csv'
# # 'inst/extdata/2025-05-27_LT1.csv'
# # 'inst/extdata/2025-09-16_LT1.csv'
# # 2025-10-01_LW1
#   # 'inst/extdata/2025-10-07_QL1.csv'
#   'inst/extdata/TROLL_exdata.csv'
#
# dat_read <- TROLL_read_data(path)
# # dplyr::glimpse(dat_read)
#
# dat_rename <- TROLL_rename_cols(df = dat_read,
#                                 strip_metadata = FALSE,
# verbose = FALSE)
# # dplyr::glimpse(dat_rename)
#
# dat_stationary <- is_stationary(df = dat_rename,
#                                 stationary_secs = 24,
#                                 # start_trim_secs = 5,
#                                 depth_range_threshold = 0.05,
#                                 drop_cols = TRUE,
#                                 plot = TRUE)
# # dplyr::glimpse(dat_stationary)
# #
# dat_stable <- TROLL_sensor_stable(df = dat_stationary,
#                                   value_col = bga_fluorescence_RFU,
#                                   # min_secs = 1,
#                                   plot = TRUE)
# # dplyr::glimpse(dat_stable)
#
# dat_summary <- TROLL_stable_summary(df = dat_stable)
# # dplyr::glimpse(dat_summary)
# dat_summary
#
# xx <-
# TROLL_profile_compiler(path = path,
#                        depth_col = depth_m,
#                        stn_secs = 24,
#                        stbl_settle_secs = 10,
#                        stbl_min_secs = 4,
#                        drop_cols = TRUE,
#                        datetimae_col = DateTime,
#                        plot = TRUE
#                        )
#
# # names(xx$Flagged_Data)
# # names(xx$Summary_Data)
