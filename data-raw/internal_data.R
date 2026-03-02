
trollCOMM_serials <- c(1151975,1153542)

troll_parameter_colnames <- c("Temperature (°C)", "Specific Conductivity (µS/cm)", "pH (pH)", "pH mV (mV)",
                        "RDO Concentration (mg/L)", "RDO Saturation (%Sat)", "Turbidity (NTU)", "ORP (mV)",
                        "Chlorophyll-a Fluorescence (RFU)", "BGA-PC Fluorescence (RFU)")

troll_metadata_colnames <- c("Pressure (psi)","Latitude (°)", "Longitude (°)", "Marked", "Date Time", "Depth (m)")

allposs_troll_colnames <- c(troll_metadata_colnames,troll_parameter_colnames)

usethis::use_data(
  trollCOMM_serials,
  troll_parameter_colnames,
  troll_metadata_colnames,
  allposs_troll_colnames,
  internal = TRUE,
  overwrite = TRUE
)
