# Songs that have been skipped the least
  
least_skipped <- function(df, top_x = 10, cutoff = 15){
  # Filter the streams that have been ended by a skip
  df <- drop_podcasts(df)
  relevant_table <- subset(df, select=c(master_metadata_track_name, master_metadata_album_artist_name, reason_end))
  out_table <- relevant_table %>% 
    group_by(master_metadata_track_name, master_metadata_album_artist_name) %>% 
    summarise(
      total_streams = n(),
      non_skips = sum(reason_end != "fwdbtn"),
    ) %>%
    mutate(
      ratio = non_skips / total_streams
    ) %>% 
    arrange(desc(ratio), desc(total_streams)) %>%
    subset(
      total_streams > cutoff
    ) %>% 
    rename(
      Track = master_metadata_track_name,
      Artist = master_metadata_album_artist_name,
      Streams = total_streams,
      Non_skips = non_skips,
      Ratio = ratio
    )
  return (head(out_table, n = top_x))
}
