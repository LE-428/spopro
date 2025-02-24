# Return the songs, that have been skipped most often (interpretation difficult,
# because these are possibly popular songs that were shuffled in too often) 
  
most_skipped <- function(data_frame, top_x = 10){
  # Filter the streams that have been ended by a skip
  relevant_table <- subset(data_frame, reason_end == "fwdbtn", select=c(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name))
  # Get the most unpopular tracks among them
  aggregated_skips <- top_tracks(relevant_table, top_x, filter_streams = FALSE)
  out <- aggregated_skips %>% 
    rename(
      Skips = Plays
    )
  return (head(out, n = top_x))
}
