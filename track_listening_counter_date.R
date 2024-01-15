# Wie oft habe ich dieses Lied gehört?
# data_table Tabelle mit Rohdaten, track_string als "string". Groß- und Kleinschreibung egal, Ausgabe in #, erstes Abspieldatum

# Beispiel

# > track_listening_counter_date(y2022, "powerade")
#       plays first_play
# 1     7     2022-05-11

track_listening_counter_date <- function(data_table, track_string, exact_search_bool = FALSE){
  if (exact_search_bool == TRUE) {
    aux_table <- data_table[(which(grepl(paste0("^", track_string, "$"), data_table$master_metadata_track_name, ignore.case=TRUE))),]
  } else if (exact_search_bool == FALSE) {
    aux_table <- data_table[(which(grepl(track_string, data_table$master_metadata_track_name, ignore.case=TRUE))),]
  }
  plays <- nrow(aux_table)
  first_date <- sort(aux_table$ts, decreasing = FALSE)
  # return(list(output,first_date[1]))
  output <- data.frame(plays = plays, first_play = substr(first_date[1], 1, 10))
  return(output)
}
