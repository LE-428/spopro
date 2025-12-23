top_artists_time_plot <- function(df, top_x = 25) {
  #' Function to plot top artists by played minutes
  #'
  #' This function plots the top `top_x` artists as vertical bars (left-to-right).
  #' Artist names are displayed vertically on the bars and the minutes are shown above.
  #'
  # Get top artists table (relies on existing top_artists function)
  
  top_tbl <- top_artists(df, top_x = top_x)
  
  # Ensure descending order (largest first, leftmost bar = largest)
  top_tbl <- top_tbl %>% arrange(desc(Playtime))
  
  artists <- as.character(top_tbl$Artist)
  minutes <- as.numeric(top_tbl$Playtime)
  
  # Plot parameters
  max_val <- max(minutes, na.rm = TRUE)
  ylim_top <- max_val * 1.20 + 1   # small space for labels
  
  # barplot
  bar_x <- barplot(
    height = minutes,
    names.arg = NA,          # suppress default x labels
    col = "grey40",
    main = paste0("Top ", length(minutes), " artists by minutes played"),
    xlab = "Artist",
    ylab = "Minutes played",
    ylim = c(0, ylim_top)
  )
  
  # Draw vertical artist labels on/near the bars
  # Place them near the bottom of the bar (5% of max height)
  label_y <- rep(max_val * 0.12, length(bar_x))
  text(
    x = bar_x,
    y = label_y,
    labels = artists,
    srt = 90,   # rotate 90 degrees (vertical)
    pos = 3,    # place text "above" the specified y (works with rotated text)
    cex = 0.9
  )
  
  # Draw minute values slightly above each bar
  value_y <- minutes + (max_val * 0.02)
  text(
    x = bar_x,
    y = value_y,
    labels = minutes,
    pos = 3,
    cex = 0.8
  )
  
  invisible(NULL)
}
