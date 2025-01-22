{
#' Funktion zur Ausgabe der gespielten Minuten nach Stunden als Plot
#'
#' Diese Funktion gibt einen Plot aus.
#'
#' @param data_table Eine Tabelle
#' @return None





{
library(dplyr)
library(roxygen2)
}


activity_time <- function(data_table){
  # print(head(data_table))
  data_table <- subset(data_table, ms_played > 30000 & conn_country == "DE", select=c(ts, ms_played)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  #print(head(data_table))
  # Beispiel: Teile des Strings extrahieren, die dem Muster entsprechen
  matches <- as.integer(regmatches(data_table$ts, regexpr("(?<=T)\\d{2}", data_table$ts, perl = TRUE)))
  # print(head(matches))
  activity_hours <- data.frame(hour = matches, ms_played=data_table$ms_played)
  # print(head(activity_hours))
  new_table <- activity_hours %>% group_by(hour) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
  
  # Vervollständige die Stunden von 0 bis 23, falls welche fehlen
  complete_hours <- data.frame(hour = 0:23)  # Alle Stunden von 0 bis 23
  new_table <- complete(complete_hours, hour) %>%
    left_join(new_table, by = "hour") %>%
    replace_na(list(total_ms_played = 0))  # Setze fehlende Stunden auf 0
  
  # Verschiebe die Stunden so, dass sie bei 3 Uhr beginnen
  shifted_data <- new_table[c(4:23, 1:3), ]
  print(shifted_data, n=24)
  # plot(shifted_data$hour, shifted_data$total_ms_played)
  #print(head(shifted_data))
  # print((shifted_data$total_ms_played))
  barplot(
    height = shifted_data$total_ms_played,                # Die Höhe der Balken
    names.arg = shifted_data$hour,               # Stunden als Namen der X-Achse
    col = "skyblue",                             # Farbe der Balken
    main = "Nach Stunden sortierte Wiedergabe",  # Titel des Plots
    xlab = "Stunde ab ...",                             # Beschriftung der X-Achse
    ylab = "Minuten gespielt",                               # Beschriftung der Y-Achse
    ylim = c(0, max(shifted_data$total_ms_played, na.rm = TRUE) + 1000)  # Y-Achse anpassen
  ) 
}


}
