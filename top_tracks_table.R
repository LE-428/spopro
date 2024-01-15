library(dplyr)
# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_tracks_table <- function(data_table, top_x){
  
  #häufigkeiten_track <- table(data_table$master_metadata_track_name)
  #sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
  #x_häufigsten_tracks <- head(sortierte_häufigkeiten, top_x)
  
  #Ohne NA's
  
  
  grouped_df <- group_by(data_table, master_metadata_track_name, master_metadata_album_artist_name)
  
  häufigkeit <- summarise(grouped_df, frequency = n())
  häufigkeit <- arrange(häufigkeit, desc(frequency))
  colnames(häufigkeit) <- c("Track","Artist","Häufigkeit")
  
  häufigkeit <- na.omit(häufigkeit)
  x_häufigsten_tracks <- head(häufigkeit, top_x)
  
  print(x_häufigsten_tracks, n = top_x)
}
  