# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

top_albums <- function(data_table, top_x){
  
  häufigkeiten_album <- table(data_table$master_metadata_album_album_name)
  sortierte_häufigkeiten <- sort(häufigkeiten_album, decreasing = TRUE)
  x_häufigsten_alben <- head(sortierte_häufigkeiten, top_x)
  # cat("Die dreißig häufigsten Werte sind:\n")
  # print(dreissig_häufigsten_alben)
  output <- x_häufigsten_alben
  return(output)
}
