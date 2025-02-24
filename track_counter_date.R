# Wie oft habe ich dieses Lied gehört?
# data_table Tabelle mit Rohdaten, track_string als "string". Groß- und Kleinschreibung egal, Ausgabe in #, erstes Abspieldatum

# Beispiel

# > track_listening_counter_date(y2022, "powerade")
#       plays first_play
# 1     7     2022-05-11


track_counter_date <- function(data_table, track_string, artist_string, exact = FALSE){
  # Escape special characters if needed
  track_string <- gsub("([()])", "\\\\\\1", track_string)
  
  # Search for the given song, exact search possible
  if (exact == TRUE) {
    aux_table <- data_table[(which(grepl(paste0("^", track_string, "$"), data_table$master_metadata_track_name, ignore.case=TRUE))),]
  } else if (exact == FALSE) {
    aux_table <- data_table[(which(grepl(track_string, data_table$master_metadata_track_name, ignore.case=TRUE))),]
  }  
   
  # Also include the artist if specified
  if(!missing(artist_string)) {
    aux_table <- aux_table[(which(grepl(artist_string, aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  
  # Counter the plays of the searched song
  plays <- nrow(aux_table)
  
  # Get the date of the first play
  first_date <- sort(aux_table$ts, decreasing = FALSE)
  
  output <- data.frame(plays = plays, first_play = substr(first_date[1], 1, 10))
  return(output)
}