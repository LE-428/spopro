{
  library(dplyr)
}



 track_time_span <- function(df, top_x=10){
  #' Return tracks with biggest timespan between first and latest completed stream
  df <- drop_podcasts(df)
  df$ts <- substr(df$ts, 1, 10)
  df <- subset(df, ms_played > 30000, select = c(ts, master_metadata_album_artist_name, master_metadata_track_name))
  
  # convert to Date so min() and max() behave correctly
  df$ts <- as.Date(df$ts)
  
  # group by artist+track, count plays and keep earliest and latest date
  result <- df %>%
    group_by(master_metadata_album_artist_name, master_metadata_track_name) %>%
    summarise(
      plays = n(),
      first_ts = min(ts),
      latest_ts = max(ts),
      .groups = "drop"
    ) %>%
    # calculate timespan between first and latest stream
    mutate(
      timespan = latest_ts - first_ts
    ) %>% 
    # order by timespan descending
    arrange(desc(timespan)) %>%
    # rename columns to something shorter and return as data.frame
    rename(
      artist = master_metadata_album_artist_name,
      track = master_metadata_track_name
    ) %>%
    # drop the artist column
    dplyr::select(-artist) %>% 
    as.data.frame()
  
  
    # converte date type into strings for Shiny application
    result$first_ts <- as.character(result$first_ts)
    result$latest_ts <- as.character(result$latest_ts)
    result$timespan <- as.character(result$timespan)
  
  print(head(result, n=top_x))
 }
 