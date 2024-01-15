# year als "string", table eine Tabelle mit Rohdaten

# Beispiel

# y2020 <- extract_year("2020", all_data)

extract_year <- function(year, table){
  output <- table[(which(grepl(year, table$ts))),]
  return(output)
}


