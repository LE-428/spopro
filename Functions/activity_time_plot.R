{
  #' Function to output the played minutes per hour as a plot
  #'
  #' This function outputs a plot.
  #'
  #' @param data_frame A table
  #' @return None
  
  
  
  
  
  {
    library(tidyr)
    library(dplyr)
    library(roxygen2)
  }
  
  
  activity_time_plot <- function(data_frame){
    # print(head(data_frame))
    data_frame <- subset(data_frame, ms_played > 30000 & conn_country == "DE", select=c(ts, ms_played)) # Filter out streams shorter than 30s; for Spotify a stream counts after 30s
    #print(head(data_frame))
    # Example: extract parts of the string that match the pattern
    matches <- as.integer(regmatches(data_frame$ts, regexpr("(?<=T)\\d{2}", data_frame$ts, perl = TRUE)))
    # print(head(matches))
    activity_hours <- data.frame(hour = matches, ms_played=data_frame$ms_played)
    # print(head(activity_hours))
    new_table <- activity_hours %>% group_by(hour) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
    
    # Complete the hours from 0 to 23 in case some are missing
    complete_hours <- data.frame(hour = 0:24)  # All hours from 0 to 23
    new_table <- complete(complete_hours, hour) %>%
      left_join(new_table, by = "hour") %>%
      replace_na(list(total_ms_played = 0))  # Set missing hours to 0
    
    # Shift the hours so that they start at 3 a.m.
    shifted_data <- new_table[c(4:24, 1:3), ]
    print("Minutes listened per time of day")
    print(shifted_data, n=24)
    
    # plot(shifted_data$hour, shifted_data$total_ms_played)
    #print(head(shifted_data))
    # print((shifted_data$total_ms_played))
    barplot(
      height = shifted_data$total_ms_played,                # Height of the bars
      names.arg = shifted_data$hour,               # Hours as labels of the X-axis
      col = "gold",                             # Color of the bars
      main = "Minutes listened per time of day",  # Title of the plot
      xlab = "Hour from ...",                             # Label of the X-axis
      ylab = "Minutes played",                               # Label of the Y-axis
      ylim = c(0, max(shifted_data$total_ms_played, na.rm = TRUE) + 1000)  # Adjust Y-axis
    ) 
  }
  
  
}
