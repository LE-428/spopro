# Show the platforms which wered used to listen to music

{
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  
  
  platform_usage_plot <- function(data_frame) {
    # Define the platforms of interest
    platforms <- c("osx", "windows", "ios", "android")
    
    # Convert the platform column to lowercase to make the search case-insensitive
    plat_lower <- tolower(data_frame$platform)
    
    # Count how often each term occurs in the column
    counts <- sapply(platforms, function(p) sum(grepl(p, plat_lower)))
    
    # Count "Other" (everything that is not in platforms)
    counts["other"] <- sum(!grepl(paste(platforms, collapse = "|"), plat_lower))
    
    # Compute the total number of entries
    total_count <- sum(counts)
    
    # Compute the percentages
    percent_counts <- (counts / total_count) * 100
    
    # Create a data frame for ggplot
    df <- data.frame(
      platform = names(percent_counts),
      percent = percent_counts
    )
    
    # Compute the midpoints of the segments (for the labels)
    df$ymax <- cumsum(df$percent)
    df$ymin <- c(0, head(df$ymax, n = -1))
    df$midpoint <- (df$ymin + df$ymax) / 2
    df$Label <- paste0(df$platform, " (", round(df$percent, 1), "%)")
    
    # Create the pie chart with labels outside the pie
    ggplot(df, aes(x = "", y = percent, fill = platform)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y", start = 0) +  # Direction around the circle
      theme_void() +  # Remove background
      labs(title = "Platform Usage Share") +
      
      # Connector lines to the labels
      # geom_segment(aes(x = 1.1, xend = 1.5, y = midpoint, yend = midpoint), 
      #              color = "black", size = 0.5) +
      
      # Labels outside the pie using `geom_label_repel`
      geom_label_repel(aes(x = 1.7, y = midpoint, label = Label), 
                       size = 4, color = "black", nudge_x = 0.1, box.padding = 0.35, segment.size = 0.5) +
      
      theme(legend.position = "none",  # Remove legend
            plot.title = element_text(hjust = 0.5))  # Center title
    
    # Optional: return the percentage values
    # return(percent_counts)
  }
  
  
}
