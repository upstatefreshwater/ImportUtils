troll_column_dictionary <- tibble::tribble(
  ~pattern,                                  ~canonical,                ~required,  ~meta,  ~core_param,  ~stbl_calc, ~derived_param, ~stability_source,

  # Required parameters
  "^Date Time$",                              "DateTime",                TRUE,      FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Depth \\(m\\)$",                          "depth_m",                 TRUE,      FALSE,       FALSE,   FALSE,      FALSE,          NA,

  # Measured parameters
  "^Specific Conductivity",                   "sp_conductivity_uScm",    FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^Temperature \\(\u00B0C\\)$",              "temperature_C",           FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^pH \\(pH\\)$",                            "pH_units",                FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^RDO Concentration",                       "DO_mgL",                  FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^Turbidity",                               "turbidity_NTU",           FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^Chlorophyll-a Fluorescence",              "chlorophyll_RFU",         FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^BGA-PC Fluorescence",                     "bga_fluorescence_RFU",    FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,
  "^ORP",                                     "ORP_mv",                  FALSE,     FALSE,       TRUE,    TRUE,       FALSE,          NA,

  # Derived Parameters
  "^RDO Saturation",                          "DO_per",                  FALSE,     FALSE,       TRUE,   FALSE,       TRUE,          'DO_mgL',
"^Chlorophyll-a Concentration( \\(µg/L\\))?$","chlorophyll_ugL",         FALSE,     FALSE,       TRUE,   FALSE,       TRUE,          'chlorophyll_RFU',    # CHECK
"^BGA-PC Concentration( \\(µg/L\\))?$",       "bga_fluorescence_ugL",    FALSE,     FALSE,       TRUE,   FALSE,       TRUE,          'bga_fluorescence_RFU',# CHECK
  "^Chlorophyll Cells",                       "chlorophyll_cells",       FALSE,     FALSE,       FALSE,  FALSE,       TRUE,          'chlorophyll_RFU',    # CHECK
  "^Total Dissolved Solids \\(ppt\\)$",       "total_diss_solids_ppt",   FALSE,     FALSE,       FALSE,  FALSE,       TRUE,          'temperature_C',      # CHECK
  "^Total Suspended Solids \\(ppt\\)$",       "total_susp_solids_ppt",   FALSE,     FALSE,       FALSE,  FALSE,       TRUE,          'temperature_C',      # CHECK
  "^FDOM Concentration",                      "FDOM",                    FALSE,     FALSE,       FALSE,  FALSE,       TRUE,          'chlorophyll_RFU',    # CHECK
  "^Crude Oil",                               "crude_oil",               FALSE,     FALSE,       FALSE,  FALSE,       TRUE,          'chlorophyll_RFU',    # CHECK


  # Additional Parameters
  "^pH mV",                                   "pH_mV",                   FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Actual Conductivity \\(\u00B5S/cm\\)$",   "actual_conductivity_uScm",FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Salinity \\(PSU\\)$",                     "salinity_psu",            FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Resistivity \\(\u03A9\u22C5cm\\)$",       "resistivity_ohmcm",       FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Density \\(g/cm\u00B3\\)$",               "density_gcm",             FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Oxygen Partial Pressure \\(Torr\\)$",     "oxy_part_press_torr",     FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,
  "^Depth to Water",                          "depth_to_water_m",        FALSE,     FALSE,       FALSE,   FALSE,      FALSE,          NA,

  # Instrument metadata
  "^External Voltage",                        "external_voltage_V",      FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Battery Capacity",                        "battery_capacity_per",    FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Pressure \\(psi\\)",                      "water_pressure_psi",      FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Barometric Pressure \\(mm Hg\\)",         "barometric_pressure_mmHg",FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Barometric Pressure \\(mbar\\)",          "barometric_pressure_mbar",FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Latitude",                                "latitude_deg",            FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Longitude",                               "longitude_deg",           FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Marked$",                                 "marked_flag",             FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA,
  "^Trollcom_temperature_C$",                 "Trollcom_temperature_C",  FALSE,     TRUE,        FALSE,   FALSE,      FALSE,          NA
)

# usethis::use_data(troll_column_dictionary,overwrite = TRUE)
