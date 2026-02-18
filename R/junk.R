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
