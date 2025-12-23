# Return table with listening time per years

# > time_year(all_le)
# # A tibble: 10 × 2
# Year  Minutes
# <chr>   <int>
#   1 2015       55
# 2 2016       18
# 3 2017       17
# 4 2019     4746
# 5 2020    21215
# 6 2021    29922
# 7 2022    29911
# 8 2023    26269
# 9 2024    20399
# 10 Total  132552

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
