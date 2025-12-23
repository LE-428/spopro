# Create a PDF listing the top 100 tracks for all years contained in the dataset
# and connecting matching tracks across years

{
  
  {
    library(jsonlite)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(tidyverse)
    library(tidyr)
    library(viridis)
    library(MASS)
    # library(hrbrthemes)
    library(GGally)
  }
  
  top_tracks_over_time_plot <- function(data = all_data, save_plot = FALSE){
    
    data$ts <- ymd_hms(data$ts)
    
    # Extract year and month from the timestamp
    data$year <- year(data$ts)
    
    data <- subset(data, data$ms_played > 30000) # Filter out streams shorter than 30s
    
    data <- data[c('year', 'master_metadata_track_name', 'master_metadata_album_artist_name')]
    result <- split(data,data$year)
    top <- list()
    
    # Iterate over all unique years in the data
    unique_years <- sort(unique(data$year))
    
    for (i in unique_years) {
      a <- result[[as.character(i)]]
      b <- top_tracks(a, 100, filter_streams = FALSE)
      b$id <- seq(1, nrow(b))
      top[[as.character(i)]] <- b
    }
    
    combined_df <- bind_rows(top, .id = "Year")
    
    # Plot
    p <- ggplot(combined_df, aes(x = factor(Year), y = id, group = Track, color = Track)) +
      geom_line(aes(x = factor(Year), y = id)) +
      geom_point(shape = 16, size = 1) + 
      geom_text(aes(label = Track), vjust = -0.5, hjust = 0.5, size = 1) +  
      labs(title = paste0("Top 100 from ", min(unique(data$year))," to ", max(unique(data$year))), 
           x = "Year", y = "Rank") +
      # theme_ipsum() +
      theme(legend.position="none",
            plot.title = element_text(hjust = 0.5),  # Center title
            plot.background = element_rect(fill = "white", color = "white")  # Set background to white
      )
    print(p)
    
    suppressWarnings(
      # Export the ggplot as high-resolution PDF
      if (save_plot == TRUE) {
        ggsave(
          filename = "top_tracks_plot.pdf",  # File name
          plot = p,                         # ggplot2 object
          device = pdf,                      # Use pdf device
          width = 12,                       # Width in inches
          height = 8,                       # Height in inches
          dpi = 300                          # Resolution (dots per inch)
        )
      }
    )
    
    # return(combined_df)
  }
  
}
