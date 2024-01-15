# Wie oft habe ich dieses Lied gehört?
# data_table Tabelle mit Rohdaten, track_string als "string". Groß- und Kleinschreibung egal, Ausgabe in Minuten

track_listening_counter <- function(track_string, data_table){
  aux_table <- data_table[(which(grepl(track_string, data_table$master_metadata_track_name, ignore.case=TRUE))),]
  output <- nrow(aux_table)
  return(output)
}
