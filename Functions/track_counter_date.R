# Count how many times a song has been played and return the date of first play
# data_frame: table with raw data
# track_string: track name as string (case-insensitive)
# artist_string: optional, filter by artist
# exact: TRUE for exact match, FALSE for partial match

# Example

# > track_listening_counter_date(y2022, "powerade")
#       plays first_play
# 1     7     2022-05-11

track_counter_date <- function(data_frame, track_string, artist_string, exact = FALSE){
  # Escape special characters if needed
  track_string <- gsub("([()])", "\\\\\\1", track_string)
  
  # Search for the given track, exact search if requested
  if (exact == TRUE) {
    aux_table <- data_frame[(which(grepl(paste0("^", track_string, "$"), data_frame$master_metadata_track_name, ignore.case=TRUE))),]
  } else {
    aux_table <- data_frame[(which(grepl(track_string, data_frame$master_metadata_track_name, ignore.case=TRUE))),]
  }  
  
  # Filter by artist if specified
  if(!missing(artist_string)) {
    aux_table <- aux_table[(which(grepl(artist_string, aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  
  # Count the plays of the track
  plays <- nrow(aux_table)
  
  # Get the date of the first play
  first_date <- sort(aux_table$ts, decreasing = FALSE)
  
  output <- data.frame(plays = plays, first_play = substr(first_date[1], 1, 10))
  return(output)
}
