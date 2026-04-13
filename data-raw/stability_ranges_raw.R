stability_ranges <- tibble::tribble(
  ~param,                                  ~range,  ~slope,

  'sp_conductivity_uScm',                   0.5,            1,
  "temperature_C",                          0.15,           0.25,
  "pH_units",                               0.1,            0.05,
  "DO_mgL",                                 0.15,           0.15,
  "turbidity_NTU",                          4.0,            0.5,
  "chlorophyll_RFU",                        1.0,            0.5,
  "bga_fluorescence_RFU",                   1.0,            0.5,
  "ORP_mv",                                 10,             1

)

# usethis::use_data(stability_ranges,
#                   overwrite = TRUE)
# #
