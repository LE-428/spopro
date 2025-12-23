# On which days did I listen to the most music?
# data_frame: data table with all columns, top_x: number of top days to return

# Example:

# y2022 <- extract_year("2022", all_data)
# > top_days_time(y2022, 5)
#   date           minutes
# 1 2022-07-29     423
# 2 2022-02-01     350
# 3 2022-01-19     338
# 4 2022-01-21     337
# 5 2022-09-10     324


top_days_time <- function(data_frame, top_x = 5){
  days_table <- subset(data_frame, select = c(ts, ms_played))
  # Extract the date and drop the HH:MM:SS part
  days_table$ts <- substr(days_table$ts, 1, 10)
  
  days_table <- days_table %>% 
    # Group by the day
    group_by(ts) %>% 
    # Sum the ms_played column
    summarise(
      Minutes = as.integer(sum(ms_played) / 60000)
    ) %>% 
    # Sort descending
    arrange(desc(Minutes)) %>% 
    rename(
      Date = ts
    ) %>% 
    as.data.frame()
  return(head(days_table, n = top_x))
}
