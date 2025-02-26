# Find out at which percentage of the duration the user is most likely to complete the stream
# Plot the listening time percentage with the fraction of songs completed at current or higher completion
{

library(dplyr)
library(ggplot2)
  
  
calculate_completion_rates <- function(df) {
  # Define percentage thresholds from 5% to 100% in steps of 5%
  percent_steps <- seq(0.05, 1, by = 0.05)
  
  # Calculate the proportion of fully played songs for each threshold
  completion_rates <- lapply(percent_steps, function(p) {
    # Filter songs that were played at least 'p' percent of their duration
    filtered <- df %>% filter(completion >= p)  
    total <- nrow(filtered)  # Total number of songs that meet the threshold
    completed <- sum(filtered$completion == 1)  # Count fully played songs
    
    # Store results as a tibble (data frame)
    tibble(Threshold = p, Completed_Ratio = completed / total)  
  }) %>% bind_rows()
  
  return(completion_rates)
}



duration_distribution_plot <- function(df_ext){
  df_ext <- drop_podcasts(df_ext)
  df_ext <- subset(df_ext, ms_played > 1000, select = c(master_metadata_album_artist_name, duration_ms, ms_played, shuffle, reason_end))
  
  # df_ext$completion = as.double(df_ext$ms_played / df_ext$duration_ms)
  df_ext$completion = as.double(ifelse(df_ext$ms_played <= df_ext$duration_ms, df_ext$ms_played / df_ext$duration_ms, NA))
  # print(head(df_ext))
  df_ext <- subset(df_ext, !is.na(completion))
  
  # Split dataframe
  df_shuffle_true <- subset(df_ext, shuffle == TRUE)
  df_shuffle_false <- subset(df_ext, shuffle == FALSE)
  
  # Calculate the proportion of fully played songs for each threshold
  # completion_rates <- calculate_completion_rates(df_ext)
  
  
  # Average dataset for comparison with df_ext
  # threshold_data <- data.frame(
  #   Threshold = seq(0.05, 1, by = 0.05),
  #   Completed_Ratio = c(0.708, 0.709, 0.716, 0.734, 0.753, 0.771, 0.788, 0.804, 
  #                       0.819, 0.833, 0.847, 0.861, 0.875, 0.888, 0.902, 0.916, 
  #                       0.932, 0.947, 0.965, 1)
  # )
  completion_rates <- calculate_completion_rates(df_shuffle_true)
  threshold_data <- calculate_completion_rates(df_shuffle_false)
  
  print(completion_rates)
  print(threshold_data)
  
  # Find the intersection point (y-value at x = 0.5)
  y_intersection <- completion_rates %>% filter(Threshold == 0.5) %>% pull(Completed_Ratio)
  y_intersection_average <- threshold_data %>% filter(Threshold == 0.5) %>% pull(Completed_Ratio)
  
  # Create the plot
  ggplot() +
    # Original completion ratio line
    geom_line(data = completion_rates, aes(x = Threshold, y = Completed_Ratio, color = "Completion Ratio"), size = 1.2) +
    geom_point(data = completion_rates, aes(x = Threshold, y = Completed_Ratio, color = "Completion Ratio"), size = 2) +
    
    # Average completion ratio line
    geom_line(data = threshold_data, aes(x = Threshold, y = Completed_Ratio, color = "Shuffle C. Ratio"), size = 1.2, linetype = "dashed") +
    geom_point(data = threshold_data, aes(x = Threshold, y = Completed_Ratio, color = "Shuffle C. Ratio"), size = 2) +
    
    # Truncated reference lines
    geom_segment(aes(x = 0.5, xend = 0.5, y = min(threshold_data$Completed_Ratio) - 0.05, yend = y_intersection), 
                 linetype = "dashed", color = "black") +
    geom_segment(aes(x = 0, xend = 0.5, y = y_intersection, yend = y_intersection), 
                 linetype = "dashed", color = "black") +
    geom_segment(aes(x = 0, xend = 0.5, y = y_intersection_average, yend = y_intersection_average), 
                 linetype = "dashed", color = "black") +
    
    # Labels and theme
    labs(title = "Completion Rate vs. Listening Time for (non-)shuffled tracks",
         x = "Minimum Completion Percentage",
         y = "Fraction of Fully Played Songs",
         color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Completion Ratio" = "blue", "Shuffle C. Ratio" = "red")) +
    theme(plot.title = element_text(hjust = 0.5),  # Center title
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}


}
