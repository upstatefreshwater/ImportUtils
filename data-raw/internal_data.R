
trollCOMM_serials <- c(1151975,1153542)

troll_column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,  ~meta,

  # Required core
  "^Date Time$",                              "DateTime",                TRUE,      FALSE,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,      FALSE,

  # Core parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,     FALSE,
  "^Temperature \\(°C\\)$",                   "temperature_C",           FALSE,     FALSE,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,     FALSE,
  "^pH mV",                                   "pH_mV",                   FALSE,     FALSE,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,     FALSE,
  "^RDO Saturation",                          "DO_per",                  FALSE,     FALSE,
  "^Turbidity",                               "turbidity_NTU",           FALSE,     FALSE,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,     FALSE,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,     FALSE,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,     FALSE,
  "^ORP",                                     "ORP_mv",                  FALSE,     FALSE,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,     TRUE,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,     TRUE,
  "^Pressure \\(psi\\)",                      "water_pressure_psi",      FALSE,     TRUE,
  "^Barometric Pressure \\(mm Hg\\)",         "barometric_pressure_mmHg",FALSE,     TRUE,
  "^Barometric Pressure \\(mbar\\)",          "barometric_pressure_mbar",FALSE,     TRUE,
  "^Latitude",                                "latitude_deg",            FALSE,     TRUE,
  "^Longitude",                               "longitude_deg",           FALSE,     TRUE,
  "^Marked$",                                 "marked_flag",             FALSE,     TRUE,
  "^Trollcom_temperature_C$",                 "Trollcom_temperature_C",  FALSE,     TRUE
)

usethis::use_data(
  trollCOMM_serials,
  troll_column_dictionary,
  internal = TRUE,
  overwrite = TRUE
)
