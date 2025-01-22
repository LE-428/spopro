# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte


# > top_albums(all_kathi, 5)
# Album_Name Play_Count
# 1 Keine Nacht für Niemand       1520
# 2                   Mit K       1453
# 3                   Drama       1316
# 4              In Schwarz       1253
# 5                    KIOX       1089


top_albums <- function(data_table, top_x = 10) {
  # Sort out the tracks with <30 seconds
  data_table <- subset(data_table, ms_played > 30000)
   
  # Count the frequency of each album
  album_frequencies <- table(data_table$master_metadata_album_album_name)
  
  # Sort the frequencies in descending order
  sorted_frequencies <- sort(album_frequencies, decreasing = TRUE)
  
  # Select the top x albums
  top_albums <- head(sorted_frequencies, top_x)
  
  # Convert the result into a data frame
  result_table <- data.frame(
    Album_Name = names(top_albums),
    Play_Count = as.integer(top_albums)
  )
  
  return(result_table)
}

