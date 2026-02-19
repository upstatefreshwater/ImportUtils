junk <-
xx %>%
  group_by(depthwholem) %>%
  arrange(id)%>%
  mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
          roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
          slope = abs(c(NA, diff(roll_mean))) ) %>%
  # Require stability AND low slope
  mutate(stable = roll_sd < 0.20 & slope < 0.10,
         DateTime = mdy_hm(DateTime))

# %>%
#   # Keep only stable points AFTER first unstable change
#   filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) %>%
#   filter(stable)%>% summarise(across( -c(DateTime, depth_halfm, id),
                                      # ~ mean(.x, na.rm = TRUE) ), .groups = "drop")

# %>%
#   select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
#                   "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
#                   "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
#                   "latitude_deg", "longitude_deg")))

junk %>% select(depth_m,roll_sd,roll_mean,slope) %>% view

ggplot(junk,aes(x = turbidity_NTU, y = depth_m)) + geom_path()

calculate_stability <- function(x) {
  if(all(is.na(x))) return(0)
  cv <- sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE))
  # Map CV to a score: lower CV = higher score
  score <- max(0, 100 * (1 - (cv / 0.05)))
  return(score)
}

data_scored <- junk %>% select(DateTime,depth_m, depthwholem,
                               temperature_C,DO_per,DO_mgL,id) %>%
  group_by(depthwholem) %>%
  arrange(id) %>%
  mutate(
    # 1. Physical movement check
    depth_velocity = abs(depth_m - lag(depth_m)),
    is_stationary = depth_velocity < 0.02,

    # 2. Rolling Stability Score (0-100)
    # Applying it to DO_mgL as the primary indicator
    stability_score = rollapply(DO_mgL, 5, calculate_stability, fill = NA, align = "right")
  ) %>%
  # 3. Filtering and Warning logic
  mutate(
    status = case_when(
      stability_score > 80 ~ "Stable",
      stability_score > 50 ~ "Settling",
      TRUE ~ "Unstable"
    )
  )
