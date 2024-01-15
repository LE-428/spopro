# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_tracks <- function(data_table = all_data, top_x){
  
  häufigkeiten_track <- table(data_table$master_metadata_track_name)
  sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
  x_häufigsten_tracks <- head(sortierte_häufigkeiten, top_x)
  # cat("Die dreißig häufigsten Werte sind:\n")
  # print(dreissig_häufigsten_alben)
  output <- x_häufigsten_tracks
  return(output)
}
