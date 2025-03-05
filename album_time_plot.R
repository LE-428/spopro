# Plot the streaming activity of an artist over time

{

library(ggplot2)
library(dplyr)
library(tidyr)


album_time_plot <- function(data_frame, album_string, artist_string, exact = FALSE){
  data_frame <- subset(data_frame, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  # Escape possible special characters in the string
  album_string <- gsub("([()])", "\\\\\\1", album_string)
  album_string <- gsub("\\$", "\\\\\\$", album_string)
  if (exact == TRUE) {
    aux_table <- data_frame[(which(grepl(paste0("^", album_string, "$"), data_frame$master_metadata_album_album_name, ignore.case=TRUE))),]
  } else if (exact == FALSE) {
    aux_table <- data_frame[(which(grepl(album_string, data_frame$master_metadata_album_album_name, ignore.case=TRUE))),]
  }  
   
  # Also include the artist if specified
  if(!missing(artist_string)) {
    aux_table <- aux_table[(which(grepl(artist_string, aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  
  aux_table$ts <- substr(aux_table$ts, 1, 7)  # Extract year and month from timestamp
  aux_table <- subset(aux_table, ms_played > 30000, select = c(ts))  # Filter songs with more than 30,000 ms played
  
  # Group by timestamp and count occurrences
  aux_table <- aux_table %>% 
    group_by(ts) %>% 
    summarise(Monthly_Plays = length(ts)) %>% 
    rename(Timestamp = ts)
  
  # Convert the Timestamp column to Date format
  aux_table$Timestamp <- as.Date(paste0(aux_table$Timestamp, "-01"), format="%Y-%m-%d")
  
  # Set default value
  if (nrow(aux_table) == 0 || all(is.na(aux_table$Timestamp))) {
    aux_table <- data.frame(Timestamp = as.Date("2023-01-01"), Monthly_Plays = 0)
  }
  
  # Create a sequence of all months within the range of the data
  all_months <- seq(min(aux_table$Timestamp, na.rm = TRUE), 
                    max(aux_table$Timestamp, na.rm = TRUE), 
                    by = "month")
  
  # Create a data frame with all months and join it with aux_table
  all_months_df <- data.frame(Timestamp = all_months)
  aux_table_full <- full_join(all_months_df, aux_table, by = "Timestamp") %>% 
    mutate(Monthly_Plays = replace_na(Monthly_Plays, 0))  # Replace NA with 0 for missing months
  
  # Plotting the streaming activity over time
  ggplot(aux_table_full, aes(x = as.Date(Timestamp, format="%Y-%m"), y = Monthly_Plays)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(title = paste("Streaming Activity for", album_string),
         x = "Month", 
         y = "Monthly Plays") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),  # Center title
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}


}
