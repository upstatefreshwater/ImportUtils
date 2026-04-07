library(ImportUtils)
library(tidyverse)
data_path <- 'inst/extdata/2025-10-07_QL1.csv'

data_read <- TROLL_read_data(data_path)
data_rename <- TROLL_rename_cols(data_read)

data_stationary <- is_stationary(data_rename,
                                 stationary_secs = 22,
                                 start_trim_secs = 4,
                                 plot = T)


chl_stable <- TROLL_sensor_stable(data_stationary,
                                  value_col = chlorophyll_RFU,
                                  plot = T) %>%
  filter(is_stationary_status == 999) %>%
  select(DateTime,stationary_depth,chlorophyll_RFU,chlorophyll_RFU_stable) %>%
  group_by(stationary_depth) %>%
  group_split()

chl_stable <- chl_stable[[6]]

bga_stable <- TROLL_sensor_stable(data_stationary,
                                  value_col = bga_fluorescence_RFU,
                                  plot = T) %>%
  filter(is_stationary_status == 999) %>%
  select(DateTime,stationary_depth,bga_fluorescence_RFU,bga_fluorescence_RFU_stable) %>%
  group_by(stationary_depth) %>%
  group_split()

bga_stable <-bga_stable[[6]]

mu <- mean(bga_stable$bga_fluorescence_RFU)
sd <- sd(bga_stable$bga_fluorescence_RFU)

cv <- sd/mu
cv

out <-
TROLL_profile_compiler(path = data_path,
                       stn_secs = 22,
                       stn_startrim_secs = 4,
                       plot = TRUE)

# openxlsx::write.xlsx(out$Summary_Data,
#                      file = 'inst/extdata/example_summary_output.xlsx')
#
# openxlsx::write.xlsx(out$Flagged_Data,
#                      file = 'inst/extdata/example_flagged_output.xlsx')

TROLL_sensor_stable(data_stationary,
                    value_col = temperature_C,
                    plot = T)
