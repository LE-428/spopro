# Alle Jahre, welche im Datensatz vorhanden sing anzeigen

get_years <- function(data_table){
  timestamps <- subset(data_table, select = c(ts))
  years <- substr(timestamps$ts, 1, 4)
  unique_years <- unique(years)
  # print(unique_years)
  print(sort(as.integer(unique_years)))
}
