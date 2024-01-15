library(dplyr)
# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_tracks_mo <- function(data_table = all_data, top_x = 10){
  
  #häufigkeiten_track <- table(data_table$master_metadata_track_name)
  #sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
  #x_häufigsten_tracks <- head(sortierte_häufigkeiten, top_x)
  
  
  
  data_table <- group_by(data_table, master_metadata_track_name, master_metadata_album_artist_name)
  
  häufigkeit <- summarise(data_table, frequency = n())
  häufigkeit <- arrange(häufigkeit, desc(frequency))
  colnames(häufigkeit) <- c("Track","Artist","Häufigkeit")
  
  # NA's wegschneiden
  häufigkeit <- na.omit(häufigkeit)
  x_häufigsten_tracks <- head(häufigkeit, top_x)
  
  return(x_häufigsten_tracks)
}

  