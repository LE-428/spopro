# Return the songs, that have been skipped most often 
  
  
most_skipped <- function(df, top_x = 10, cutoff = 15){
  # Filter the streams that have been ended by a skip
  df <- drop_podcasts(df)
  relevant_table <- subset(df, select=c(master_metadata_track_name, master_metadata_album_artist_name, reason_end))
  out_table <- relevant_table %>% 
    group_by(master_metadata_track_name, master_metadata_album_artist_name) %>% 
    summarise(
      total_streams = n(),
      skips = sum(reason_end == "fwdbtn"),
    ) %>%
    mutate(
      ratio = skips / total_streams
    ) %>% 
    arrange(desc(ratio), desc(total_streams)) %>%
    subset(
      total_streams > cutoff
    ) %>% 
    rename(
      Track = master_metadata_track_name,
      Artist = master_metadata_album_artist_name,
      Streams = total_streams,
      Skips = skips,
      Ratio = ratio
    )
  return (head(out_table, n = top_x))
}
