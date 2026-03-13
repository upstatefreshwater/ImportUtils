
# Check that sensor data exist
# Extract parameter column names in df
params <- names(df)[which(names(df) %in% troll_column_dictionary$canonical[troll_column_dictionary$core_param])]

if(length(params) <1 ){
  stop('No sensor data columns identified. Column names must be standardized using the "TROLL_rename_cols" function.')
}
