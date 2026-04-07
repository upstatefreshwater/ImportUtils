library(ImportUtils)
data_path <- 'inst/extdata/2025-10-07_QL1.csv'

data_read <- TROLL_read_data(data_path)
data_rename <- TROLL_rename_cols(data_read)

data_stationary <- is_stationary(data_rename,
                                 stationary_secs = 22,
                                 start_trim_secs = 4,
                                 plot = T)


