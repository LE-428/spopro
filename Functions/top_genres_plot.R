# Pie chart with the 10 most popular genres

{
  
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  
  top_genres_plot <- function(data_table_ext, top_x = 10){
    result <- top_genres(data_table_ext, top_x = top_x)
    # Calculate total count
    total <- sum(result$Count)
    
    # Reverse order so labels are arranged in correct clockwise direction
    result <- result %>%
      arrange(desc(Count)) %>%
      mutate(Percentage = Count / total * 100,
             Label = paste0(Genre, " (", round(Percentage, 1), "%)"),
             Cumulative = rev(cumsum(rev(Count))),  # Reverse order
             Midpoint = Cumulative - Count / 2)  # Midpoint of each segment
    
    # Pie chart with correct clockwise direction
    ggplot(result, aes(x = "", y = Count, fill = Genre)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y", start = 0, direction = -1) +  # Reverse direction
      theme_void() +  # Remove background
      labs(title = "Top Genres Pie Chart") +
      
      # Connecting lines to labels
      geom_segment(aes(x = 1.3, xend = 1.7, y = Midpoint, yend = Midpoint), 
                   color = "black", size = 0.5) +
      
      # Labels outside the pie
      geom_label_repel(aes(x = 2, y = Midpoint, label = Label), 
                       size = 4, color = "black", hjust = 0.5) +
      
      theme(legend.position = "none",  # Remove legend, labels already displayed
            plot.title = element_text(hjust = 0.5))
  }
  
}
