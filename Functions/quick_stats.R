# Quick overview of the dataset

{
  library(jsonlite)
  
  quick_stats <- function(data_frame) {
    
    num_tracks <- length(unique(data_frame$spotify_track_uri))
    num_artists <- length(unique(data_frame$master_metadata_album_artist_name))
    
    countries <- sort(table(data_frame$conn_country), decreasing = TRUE)
    num_countries <- length(countries)
    
    timestamps <- subset(data_frame, select = c(ts))
    years <- substr(timestamps$ts, 1, 4)
    unique_years <- unique(years)
    # print(unique_years)
    
    result <- paste(
      "Years found in the dataset:", min(unique_years), "-", max(unique_years), "\n",
      "Number of distinct tracks:", num_tracks, "\n",
      "Number of distinct artists:", num_artists, "\n",
      "Number of countries that you listened music in:", num_countries, "\n"
    )
    
    
    # If in console, print it out
    if (interactive()) cat(result, "\n")
    
    return(invisible(result))
  }
  
}
