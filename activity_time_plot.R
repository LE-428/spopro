{
#' Funktion zur Ausgabe der gespielten Minuten nach Stunden als Plot
#'
#' Diese Funktion gibt einen Plot aus.
#'
#' @param data_frame Eine Tabelle
#' @return None





{
library(tidyr)
library(dplyr)
library(roxygen2)
}


activity_time_plot <- function(data_frame){
  # print(head(data_frame))
  data_frame <- subset(data_frame, ms_played > 30000 & conn_country == "DE", select=c(ts, ms_played)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  #print(head(data_frame))
  # Beispiel: Teile des Strings extrahieren, die dem Muster entsprechen
  matches <- as.integer(regmatches(data_frame$ts, regexpr("(?<=T)\\d{2}", data_frame$ts, perl = TRUE)))
  # print(head(matches))
  activity_hours <- data.frame(hour = matches, ms_played=data_frame$ms_played)
  # print(head(activity_hours))
  new_table <- activity_hours %>% group_by(hour) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
  
  # Vervollständige die Stunden von 0 bis 23, falls welche fehlen
  complete_hours <- data.frame(hour = 0:24)  # Alle Stunden von 0 bis 23
  new_table <- complete(complete_hours, hour) %>%
    left_join(new_table, by = "hour") %>%
    replace_na(list(total_ms_played = 0))  # Setze fehlende Stunden auf 0
  
  # Verschiebe die Stunden so, dass sie bei 3 Uhr beginnen
  shifted_data <- new_table[c(4:24, 1:3), ]
  print("Minutes listened per time of day")
  print(shifted_data, n=24)
  
  # plot(shifted_data$hour, shifted_data$total_ms_played)
  #print(head(shifted_data))
  # print((shifted_data$total_ms_played))
  barplot(
    height = shifted_data$total_ms_played,                # Die Höhe der Balken
    names.arg = shifted_data$hour,               # Stunden als Namen der X-Achse
    col = "gold",                             # Farbe der Balken
    main = "Minutes listened per time of day",  # Titel des Plots
    xlab = "Hour from ...",                             # Beschriftung der X-Achse
    ylab = "Minutes played",                               # Beschriftung der Y-Achse
    ylim = c(0, max(shifted_data$total_ms_played, na.rm = TRUE) + 1000)  # Y-Achse anpassen
  ) 
}


}
