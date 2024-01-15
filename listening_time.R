# data_table Tabelle mit Rohdaten, Ausgabe in Minuten

#Beispiel

# > listening_time(y2022)
# [1] 35133


listening_time <- function(data_table){
  output <- round(sum(data_table$ms_played) / 60000, digits = 0)
  return(output)
}
