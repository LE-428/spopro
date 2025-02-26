# data_frame Tabelle mit Rohdaten, artist_string als "string". Groß- und Kleinschreibung egal, Ausgabe in Minuten

# Beispiel

# > artist_listening_time(y2023, "metro boomin")
# [1] 368


artist_listening_time <- function(data_frame, artist_string) {
  aux_table <-
    data_frame[(which(
      grepl(
        artist_string,
        data_frame$master_metadata_album_artist_name,
        ignore.case = TRUE
      )
    )), ]
  output <- round(sum(aux_table$ms_played) / 60000, digits = 0)
  return(output)
}
