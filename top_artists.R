# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_artists <- function(data_table = all_data, top_x){
  
  häufigkeiten_artists <- table(data_table$master_metadata_album_artist_name)
  sortierte_häufigkeiten <- sort(häufigkeiten_artists, decreasing = TRUE)
  x_häufigsten_artists <- head(sortierte_häufigkeiten, top_x)
  # cat("Die dreißig häufigsten Werte sind:\n")
  # print(dreissig_häufigsten_alben)
  output <- x_häufigsten_artists
  return(output)
}
