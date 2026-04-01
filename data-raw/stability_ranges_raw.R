stability_ranges <- tibble::tribble(
  ~param,                                  ~range_thresh,

  'sp_conductivity_uScm',                   0.5,
  "temperature_C",                          0.15,
  "pH_units",                               0.1,
  "DO_mgL",                                 0.15,
  "turbidity_NTU",                          0.5,
  "chlorophyll_RFU",                        0.3,
  "bga_fluorescence_RFU",                   0.3,
  "ORP_mv",                                 10

)

# usethis::use_data(stability_ranges,
#                   overwrite = TRUE)
# #
