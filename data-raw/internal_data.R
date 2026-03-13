
trollCOMM_serials <- c(1151975,1153542)

troll_column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,  ~meta,  ~core_param,

  # Required core
  "^Date Time$",                              "DateTime",                TRUE,      FALSE,       FALSE,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,      FALSE,       FALSE,

  # Core parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,     FALSE,       TRUE,
  "^Temperature \\(°C\\)$",                   "temperature_C",           FALSE,     FALSE,       TRUE,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,     FALSE,       TRUE,
  "^pH mV",                                   "pH_mV",                   FALSE,     FALSE,       FALSE,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,     FALSE,       TRUE,
  "^RDO Saturation",                          "DO_per",                  FALSE,     FALSE,       TRUE,
  "^Turbidity",                               "turbidity_NTU",           FALSE,     FALSE,       TRUE,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,     FALSE,       TRUE,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,     FALSE,       TRUE,
  "^ORP",                                     "ORP_mv",                  FALSE,     FALSE,       TRUE,
  "^Actual Conductivity \\(µS/cm\\)$",        "actual_conductivity_uScm",FALSE,     FALSE,       TRUE,

  # Additional Parameters
  "^Salinity \\(PSU\\)$",                     "salinity_psu",            FALSE,     FALSE,       FALSE,
  "^Resistivity \\(Ω⋅cm\\)$",                 "resistivity_ohmcm",       FALSE,     FALSE,       FALSE,
  "^Density \\(g/cm³\\)$",                    "density_gcm",             FALSE,     FALSE,       FALSE,
  "^Total Dissolved Solids \\(ppt\\)$",       "total_diss_solids_ppt",   FALSE,     FALSE,       FALSE,
  "^Oxygen Partial Pressure \\(Torr\\)$",     "oxy_part_press_torr",     FALSE,     FALSE,       FALSE,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,     FALSE,       FALSE,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,     TRUE,       FALSE,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,     TRUE,       FALSE,
  "^Pressure \\(psi\\)",                      "water_pressure_psi",      FALSE,     TRUE,       FALSE,
  "^Barometric Pressure \\(mm Hg\\)",         "barometric_pressure_mmHg",FALSE,     TRUE,       FALSE,
  "^Barometric Pressure \\(mbar\\)",          "barometric_pressure_mbar",FALSE,     TRUE,       FALSE,
  "^Latitude",                                "latitude_deg",            FALSE,     TRUE,       FALSE,
  "^Longitude",                               "longitude_deg",           FALSE,     TRUE,       FALSE,
  "^Marked$",                                 "marked_flag",             FALSE,     TRUE,       FALSE,
  "^Trollcom_temperature_C$",                 "Trollcom_temperature_C",  FALSE,     TRUE,       FALSE,
)
troll_serial_numbers <- c("1265014", "1265071", "1265303", "1265329", "1265335", "1265124",
                          "1265022", "1265304", "1265301", "1265338", "1155867", "1217315",
                          "1217436", "1217270", "1217311", "1217355", "1152515", "1265302"
)


stability_ranges <- tibble::tribble(
  ~param,                                  ~range_thresh,

  'sp_conductivity_uScm',                   0.5,
  "temperature_C",                          0.15,
  "pH_units",                               0.1,
  "DO_mgL",                                 0.15,
  "DO_per",                                 NA,
  "turbidity_NTU",                          0.5,
  "chlorophyll_RFU",                        0.3,
  "bga_fluorescence_RFU",                   0.3,
  "ORP_mv",                                 10

)
usethis::use_data(
  trollCOMM_serials,
  troll_serial_numbers,
  troll_column_dictionary,
  internal = TRUE,
  overwrite = TRUE
)
