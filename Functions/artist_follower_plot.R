# Plot the number of followers of the listened artists
# requires data_table_ext

artist_follower_plot <- function(data_table_ext, sort_by_streams = TRUE){
  # Filter out short streams and select relevant columns
  follower_table <- subset(data_table_ext, data_table_ext$ms_played > 30000, select = c(artist_id, artist_followers))
  unique_artists <- unique(follower_table)
  # print(head(unique_artists))
  if (sort_by_streams == FALSE) {
    # Convert the number of followers into orders of magnitude using the logarithm
    followers_scaled <- as.integer(log10(unique_artists$artist_followers))
    ylabel = "Number of artists in the dataset"
  } else {
    followers_scaled <- as.integer(log10(follower_table$artist_followers))
    ylabel = "Streams by artists"
  }
  # print(head(followers_scaled))
  # Count occurring orders of magnitude
  scaled_frequencies <- table(followers_scaled)
  # print(scaled_frequencies)
  
  # Create categories based on the logarithm
  unique_scales <- sort(unique(followers_scaled))
  lower_bound <- 10^unique_scales  # Lower bound of each interval (e.g. 10^1 = 10, 10^2 = 100)
  upper_bound <- 10^(unique_scales + 1)  # Upper bound of each interval (e.g. 10^2 = 100, 10^3 = 1000)
  
  # Create axis labels as "1-10", "10-100", "100-1000", ...
  interval_labels <- paste0(lower_bound, "-", upper_bound)
  # print(interval_labels)
  
  # # Bar plot with the distribution of follower counts in logarithmic intervals
  barplot_heights <- barplot(
    height = scaled_frequencies,                # Frequencies as bar heights
    #names.arg = interval_labels,      # Interval labels for the X-axis
    col = "violet",                          # Color of the bars
    main = "Artist",  # Title of the plot
    xlab = "Number of followers, logarithmic",                            # X-axis label
    ylab = ylabel,         # Y-axis label
    ylim = c(0, max(scaled_frequencies) + 10),     # Adjust Y-axis
    #las = 2  # Rotate axis labels if they are too long
    xaxt = "n"
  )
  
  # Place the X-axis labels inside the bars
  text(
    x = barplot_heights + 0.2,                          # X-position of the bars
    y = rep(0.1 * max(scaled_frequencies), length(barplot_heights)),                 # Y-position inside the bars
    labels = interval_labels,                     # Generated labels
    srt = 90,                                      # Do not rotate labels
    pos = 3,                                      # Position inside the bars
    cex = 0.8                                      # Font size
  )
}
