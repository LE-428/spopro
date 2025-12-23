# Computes listening time from dataframe, return value in minutes

# Example

# y2020 <- extract_year("2020", all_data)
# > listening_time(y2020)
# [1] 35133


listening_time <- function(data_frame){
  output <- round(sum(data_frame$ms_played) / 60000, digits = 0)
  return(output)
}
