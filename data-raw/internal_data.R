
trollCOMM_serials <- c(1151975,1153542)

troll_parameter_colnames <- c("Temperature (°C)", "Specific Conductivity (µS/cm)", "pH (pH)", "pH mV (mV)",
                        "RDO Concentration (mg/L)", "RDO Saturation (%Sat)", "Turbidity (NTU)", "ORP (mV)",
                        "Chlorophyll-a Fluorescence (RFU)", "BGA-PC Fluorescence (RFU)")

troll_metadata_colnames <- c("Pressure (psi)","Latitude (°)", "Longitude (°)", "Marked", "Date Time", "Depth (m)")

allposs_troll_colnames <- c(troll_metadata_colnames,troll_parameter_colnames)

troll_column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,

  # Required core
  "^Date Time$",                             "DateTime",                TRUE,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,

  # Core parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,
  "^Temperature \\(°C\\)$",                   "temperature_C",           FALSE,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,
  "^pH mV",                                   "pH_mV",                   FALSE,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,
  "^RDO Saturation",                          "DO_per",                  FALSE,
  "^Turbidity",                               "turbidity_NTU",           FALSE,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,
  "^Barometric Pressure",                     "barometric_pressure_mbar",FALSE,
  "^Latitude",                                "latitude_deg",            FALSE,
  "^Longitude",                               "longitude_deg",           FALSE,
  "^Marked$",                                 "marked_flag",             FALSE,
  "^Trollcom_temperature_C$",                     "Trollcom_temperature_C",  FALSE
)

usethis::use_data(
  trollCOMM_serials,
  troll_parameter_colnames,
  troll_metadata_colnames,
  allposs_troll_colnames,
  internal = TRUE,
  overwrite = TRUE
)
