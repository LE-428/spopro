# Return table with listening time per years

time_year <- function(df) {
  # Extract year from the timestamp
  df$ts <- substr(df$ts, 1, 4)
  
  # Group by year and calculate the minutes
  df <- df %>% 
    group_by(ts) %>% 
    summarise(
      Minutes = as.integer(sum(ms_played) / 60000)
    ) %>% 
    rename(
      Year = ts
    ) %>% 
    arrange(-desc(Year))
  
  # Calculate the total minutes across all years
  total_minutes <- sum(df$Minutes)
  
  # Create a row with the "Total" label and the total minutes
  total_row <- data.frame(Year = "Total", Minutes = total_minutes)
  
  # Add the "Total" row to the bottom of the dataframe
  df <- bind_rows(df, total_row)
  
  # Print the updated dataframe
  print(df)
}
