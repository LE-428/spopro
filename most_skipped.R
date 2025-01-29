# Return the songs, that have been skipped most often (interpretation difficult,
# because these are possibly popular songs that were shuffled in too often) 
  
{
  
top_tracks <- function(data_table = all_data, top_x = 5){
  
  häufigkeiten_track <- table(data_table$master_metadata_track_name)
  sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
  x_häufigsten_tracks <- head(sortierte_häufigkeiten, top_x)
  # cat("Die dreißig häufigsten Werte sind:\n")
  # print(dreissig_häufigsten_alben)
  output <- x_häufigsten_tracks
  return(output)
}
  

most_skipped <- function(data_table, top_x = 10){
  relevant_table <- subset(data_table, reason_end == "fwdbtn", select=c(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name))
  aggregated_skips <- top_tracks(relevant_table, top_x)
  out <- data.frame(Track = names(aggregated_skips), skipped = as.integer(aggregated_skips))
  # print(head(relevant_table))
  return(out)
}
}
