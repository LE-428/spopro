# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_album_tracks <- function(data_table, album_string, artist_string){
  data_table <- subset(data_table, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s 
  
  aux_table <- data_table[(which(grepl(album_string, data_table$master_metadata_album_album_name, ignore.case=TRUE))),]
  
  if(!missing(artist_string)) {
    aux_table <- aux_table[(which(grepl(artist_string, aux_table$master_metadata_album_artist_name, ignore.case=TRUE))),]
  }
  
  track_plays <- table(aux_table$master_metadata_track_name)
  
  sortierte_häufigkeiten <- sort(track_plays, decreasing = TRUE)
  return(sortierte_häufigkeiten)
}
