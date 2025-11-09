# Plot histogram of the number of tracks played per day

library(dplyr)
library(ggplot2)

tracks_per_day_plot <- function(data_table) {
  # Keep only tracks with more than 60 seconds of playtime
  data_table <- subset(data_table, ms_played > 60000, 
                       select = c(master_metadata_album_artist_name, master_metadata_track_name, ts))
  
  # Extract date from timestamp (first 10 characters)
  data_table$ts <- substr(data_table$ts, 1, 10)
  
  # print(head(data_table))
  
  # Group by date and count number of tracks per day
  daily_counts <- data_table %>%
    group_by(ts) %>%
    summarise(tracks_played = n(), .groups = "drop")
  
  # print(head(daily_counts))
  
  # compute candidate upper bound using the 'frequency' idea
  freq_tbl <- table(daily_counts$tracks_played)
  vals_more2 <- as.numeric(names(freq_tbl)[freq_tbl > 2])
  
  if (length(vals_more2) > 0) {
    x_upper <- max(vals_more2) + 50
  } else {
    x_upper = max(daily_counts$tracks_played)
  }
  
  # Plot histogram of the number of tracks played per day
  ggplot(daily_counts, aes(x = tracks_played)) +
    geom_histogram(binwidth = 5, color = "black", fill = "orchid4") +
    xlim(0, x_upper) +
    labs(title = "Histogram of Tracks Played per Day",
         x = "Number of Tracks per Day (grouped by 5)",
         y = "Frequency (number of days)") +
    theme_minimal() +
    theme(legend.position = "none",  # remove legend 
          plot.title = element_text(hjust = 0.5))  # center title
}
