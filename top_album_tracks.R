# data_frame Tabelle mit Rohdaten, album_string: Name des Albums, artist_string: Name des Künstlers, 
# exact_search_bool = TRUE, falls der Albumname exakt eingegeben wird


# > top_album_tracks(all_le, "ye", "Kanye West", 1)
# Track_Name Play_Count
# 1                       Yikes         10
# 2                  Ghost Town          6
# 3 I Thought About Killing You          4
# 4                 No Mistakes          4
# 5              Violent Crimes          4
# 6                    All Mine          3
# 7              Wouldn't Leave          2

top_album_tracks <- function(data_frame, album_string, artist_string, exact_search_bool = FALSE){
  data_frame <- subset(data_frame, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s 
  album_string <- gsub("([()])", "\\\\\\1", album_string)
  album_string <- gsub("\\$", "\\\\\\$", album_string)
  
  if (exact_search_bool == TRUE) {
    aux_table <- data_frame[(which(grepl(paste0("^", album_string, "$"), data_frame$master_metadata_album_album_name, ignore.case=TRUE))),]
  } else if (exact_search_bool == FALSE) {
    aux_table <- data_frame[(which(grepl(album_string, data_frame$master_metadata_album_album_name, ignore.case=TRUE))),]
  }
  #aux_table <- data_frame[(which(grepl(album_string, data_frame$master_metadata_album_album_name, ignore.case=TRUE)x = )),]
  
  if(!missing(artist_string)) {
    aux_table <- aux_table[(which(grepl(artist_string, aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  
  track_plays <- table(aux_table$master_metadata_track_name)
  
  sorted_frequencies <- sort(track_plays, decreasing = TRUE)
  # In Tabelle umwandeln
  result_table <- data.frame(
    Track_Name = names(sorted_frequencies),
    Play_Count = as.integer(sorted_frequencies)
  )
  return(result_table)
}

