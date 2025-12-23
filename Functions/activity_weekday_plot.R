# Plot the streams distributed over the weekdays

activity_weekday_plot <- function(data_frame){
  # Filter out streams shorter than 30 seconds
  data_frame <- subset(
    data_frame,
    ms_played > 30000,
    select = c(ts, ms_played)
  )
  
  # Parse timestamps: 2015-10-09T15:25:14Z to weekday
  time_parsed <- as.POSIXct(
    data_frame$ts,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  
  # Extract weekday (Monday = 1, Sunday = 7)
  weekdays_num <- as.integer(
    format(time_parsed, "%u")
  )
  
  activity_weekdays <- data.frame(
    weekday = weekdays_num,
    ms_played = data_frame$ms_played
  )
  
  new_table <- activity_weekdays %>%
    group_by(weekday) %>%
    summarise(
      total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60),
      .groups = "drop"
    )
  
  # Insert all weekdays (Monday to Sunday)
  complete_weekdays <- data.frame(weekday = 1:7)
  
  new_table <- complete_weekdays %>%
    left_join(new_table, by = "weekday") %>%
    replace_na(list(total_ms_played = 0))
  
  print("Minutes played by weekday")
  print(new_table)
  
  # Weekday labels (Monday to Sunday)
  weekday_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  
  barplot(
    height = new_table$total_ms_played,
    names.arg = weekday_labels,
    col = "plum4",
    main = "Minutes played by weekday",
    xlab = "Weekday",
    ylab = "Minutes played",
    ylim = c(
      0,
      max(new_table$total_ms_played, na.rm = TRUE) + 1000
    )
  )
}
