# Return the songs, that have been skipped most often (interpretation difficult,
# because these are possibly popular songs that were shuffled in too often) 
  
{

most_skipped <- function(data_table, top_x = 10){
  relevant_table <- subset(data_table, reason_end == "fwdbtn", select=c(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name))
  aggregated_skips <- top_tracks(relevant_table, top_x)
  # print(head(relevant_table))
  print(aggregated_skips)
}
}
