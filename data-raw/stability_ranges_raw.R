stability_ranges <- tibble::tribble(
  ~param,                                   ~range,         ~slope,

  # Measured Parameters
  'sp_conductivity_uScm',                   0.5,            1,
  "temperature_C",                          0.15,           0.25,
  "pH_units",                               0.1,            0.05,
  "DO_mgL",                                 0.15,           0.15,
  "turbidity_NTU",                          4.0,            0.5,
  "chlorophyll_RFU",                        1.0,            0.5,
  "bga_fluorescence_RFU",                   1.0,            0.5,
  "ORP_mv",                                 10,             1,

  # Derived Parameters
  "DO_per",                                 3.0,              5.0,
  "chlorophyll_ugL",                        1.5,            1.0,
  "bga_fluorescence_ugL",                   1.0,              0.75,
  "chlorophyll_cells",                      5000,           5000,
  "total_diss_solids_ppt",                  15,             10,
  "total_susp_solids_ppt",                  20,             15,
  "FDOM",                                   1.0,            0.5,
  "crude_oil",                              10,             5
)

# usethis::use_data(stability_ranges,
#                   overwrite = TRUE)
#
