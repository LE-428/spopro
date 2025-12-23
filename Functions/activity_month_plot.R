{
  #' Function to output the played minutes per month as a plot
  #'
  #' This function outputs a plot.
  #'
  #' @param data_frame A table
  #' @return None
  
  
  
  
  
  {
    library(dplyr)
    library(roxygen2)
  }
  
  
  activity_month_plot <- function(data_frame){
    # print(head(data_frame))
    data_frame <- subset(data_frame, ms_played > 30000, select=c(ts, ms_played)) # Filter out streams shorter than 30s; for Spotify a stream counts after 30s
    #print(head(data_frame))
    # Example: extract parts of the string that match the pattern
    matches <- as.integer(regmatches(data_frame$ts, regexpr("(?<=\\d{4}-)\\d{2}", data_frame$ts, perl = TRUE)))
    # print(head(matches))
    activity_months <- data.frame(month = matches, ms_played=data_frame$ms_played)
    # print(head(activity_hours))
    new_table <- activity_months %>% group_by(month) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
    
    # Insert the twelve months
    complete_months <- data.frame(month = 1:12)  # all months
    new_table <- complete(complete_months, month) %>%
      left_join(new_table, by = "month") %>%
      replace_na(list(total_ms_played = 0))  # Set missing months to 0
    
    print("Minutes played by month")
    print(new_table, n=12)
    # plot(new_table$hour, new_table$total_ms_played)
    #print(head(new_table))
    # print((new_table$total_ms_played))
    
    # Replace the month numbers (1 to 12) with JFMAMJJASOND
    month_labels <- strsplit("JFMAMJJASOND", "")[[1]]
    
    barplot(
      height = new_table$total_ms_played,                # Height of the bars
      names.arg = month_labels,               # Months as labels of the X-axis
      col = "orangered",                             # Color of the bars
      main = "Minutes played by month",  # Title of the plot
      xlab = "Month",                             # Label of the X-axis
      ylab = "Minutes played",                               # Label of the Y-axis
      ylim = c(0, max(new_table$total_ms_played, na.rm = TRUE) + 1000)  # Adjust Y-axis
    ) 
  }
  
  
}
