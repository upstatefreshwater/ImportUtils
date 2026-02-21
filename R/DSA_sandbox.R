data_final <- data%>% group_by(depthwholem)%>%arrange(id)%>%
  mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
          roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
          slope = abs(c(NA, diff(roll_mean))) )%>%
  # Require stability AND low slope
  mutate(stable = roll_sd < 0.20 & slope < 0.10) %>%
  # Keep only stable points AFTER first unstable change
  filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) %>%
  filter(stable)%>% summarise(across( -c(DateTime, depth_halfm, id),
                                      ~ mean(.x, na.rm = TRUE) ), .groups = "drop")%>%
  select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
                  "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                  "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                  "latitude_deg", "longitude_deg")))

param_col <- c("Date Time", "Depth (m)", "Temperature (°C)", "Specific Conductivity (µS/cm)", "pH (pH)", "pH mV (mV)",
               "RDO Concentration (mg/L)", "RDO Saturation (%Sat)", "Turbidity (NTU)", "ORP (mV)",
               "Chlorophyll-a Fluorescence (RFU)", "BGA-PC Fluorescence (RFU)", "Pressure (psi)",
               "Latitude (°)", "Longitude (°)", "Marked")
param_rename <- c(DateTime = "Date Time", depth_m = "Depth (m)", temperature_C = "Temperature (°C)",
                  sp_conductivity_uScm = "Specific Conductivity (µS/cm)", pH_units = "pH (pH)",
                  pH_mV = "pH mV (mV)", DO_mgL = "RDO Concentration (mg/L)", DO_per = "RDO Saturation (%Sat)",
                  turbidity_NTU = "Turbidity (NTU)", ORP_mV = "ORP (mV)",
                  chlorophyll_RFU = "Chlorophyll-a Fluorescence (RFU)", bga_fluorescence_RFU = "BGA-PC Fluorescence (RFU)",
                  pressure_psi = "Pressure (psi)", latitude_deg = "Latitude (°)", longitude_deg = "Longitude (°)")


dat <- read_datafile('inst/extdata/2025-05-27_LT1.csv')
dat <- read_datafile('inst/extdata/2025-09-16_LT1.csv')
# dat <- read_datafile('inst/extdata/2025-05-13_LW1.csv')
# dat <- read_datafile('inst/extdata/2025-10-01_LW1.csv')


dat <- rename_cols(dat) |>
  dplyr::mutate(depthwholem = (janitor::round_half_up(depth_m)))

try <- is_stationary(dat,drop_cols = F,
                     plot = T)



plotstuff <- function(df,param){
  trash <- df %>% ungroup() %>%
    select(DateTime,depth_m,depthwholem,!!sym(param)) %>%
    mutate(x = c(depthwholem - lag(depthwholem,default = 0)),
           label = ifelse(x==1,depthwholem,NA))

  ggplot(trash) +
    geom_point(aes(x = DateTime, y = !!sym(param))) +
    geom_label(aes(x = DateTime, y = !!sym(param),label = label),
               nudge_y = 0.25)
}

plotstuff(df = dat,'temperature_C')
plotstuff(df = dat,'DO_mgL')
plotstuff(df = dat, 'depth_m')


