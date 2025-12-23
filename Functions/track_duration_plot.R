# Plot the duration of listened tracks
# data_table_ext required

track_duration_plot <- function(data_table_ext, sort_by_streams = TRUE) {
  # Use fully played tracks and select relevant columns
  listened_tracks <-
    subset(
      data_table_ext,
      data_table_ext$ms_played == data_table_ext$duration_ms,
      select = c(spotify_track_uri, duration_ms)
    )
  unique_tracks <- unique(listened_tracks)
  
  # Count each distinct track once or count every stream
  if (sort_by_streams == FALSE) {
    # Divide durations into 30s intervals using floor division
    duration_classes <- unique_tracks$duration_ms %/% (30 * 1000)
    ylabel = "Number of unique songs"
  } else {
    duration_classes <- listened_tracks$duration_ms %/% (30 * 1000)
    ylabel = "Number of streams"
  }
  
  # Count the occurrences of each duration class
  duration_frequencies <- table(duration_classes)
  # print(duration_frequencies)
  
  # Create axis labels like "0-30", "30-60", ...
  interval_labels <- paste0(as.numeric(names(duration_frequencies)) * 30, "-", 
                            (as.numeric(names(duration_frequencies)) + 1) * 30)
  
  # Barplot of the distribution of song lengths in 30s intervals
  barplot_heights <- barplot(
    height = duration_frequencies,          # Heights of bars
    col = "plum",                           # Bar color
    main = "Distribution of song length",  # Plot title
    xlab = "Duration in seconds",           # X-axis label
    ylab = ylabel,                          # Y-axis label
    ylim = c(0, max(duration_frequencies) + 10), # Adjust Y-axis
    xaxt = "n"                              # Hide default X-axis
  )
  
  # Place X-axis labels within bars
  text(
    x = barplot_heights + 0.2,                     # X position of bars
    y = rep(0.05 * max(duration_frequencies), length(barplot_heights)), # Y position at 5% of max
    labels = interval_labels,                      # The created labels
    srt = 90,                                      # Rotate labels 90 degrees
    pos = 3,                                       # Position relative to coordinates
    cex = 0.8                                      # Font size
  )
}
