# Plot the distribution of the completion rate of all listened songs

library(ggplot2)

completion_rate_plot <- function(data_table_extended) {
  # Keep only songs with more than 1 second of playtime
  data_table_extended <- subset(data_table_extended, ms_played > 1000, 
                                select = c(ms_played, duration_ms))
  
  # Compute completion rate (listening time / song duration)
  data_table_extended$completion_rate <- data_table_extended$ms_played / data_table_extended$duration_ms
  
  # Remove non-finite and keep only 0..1
  data_table_extended <- subset(data_table_extended, is.finite(completion_rate) & completion_rate >= 0 & completion_rate <= 1)
  
  print(head(data_table_extended))
  
  # Plot histogram of completion rates in 5% steps
  ggplot(data_table_extended, aes(x = completion_rate)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "sienna2") +
    scale_x_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.1)) +
    labs(title = "Distribution of Song Completion Rates",
         x = "Completion Rate (5% steps)",
         y = "Number of Songs") +
    theme_minimal() +
    theme(legend.position = "none",  # remove legend
          plot.title = element_text(hjust = 0.5))  # center title
}
