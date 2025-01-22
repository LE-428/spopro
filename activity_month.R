{
  #' Funktion zur Ausgabe der gespielten Minuten nach Monaten als Plot
  #'
  #' Diese Funktion gibt einen Plot aus.
  #'
  #' @param data_table Eine Tabelle
  #' @return None
  
  
  
  
  
{
  library(dplyr)
  library(roxygen2)
}
  
  
activity_month <- function(data_table){
  # print(head(data_table))
  data_table <- subset(data_table, ms_played > 30000, select=c(ts, ms_played)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  #print(head(data_table))
  # Beispiel: Teile des Strings extrahieren, die dem Muster entsprechen
  matches <- as.integer(regmatches(data_table$ts, regexpr("(?<=\\d{4}-)\\d{2}", data_table$ts, perl = TRUE)))
  # print(head(matches))
  activity_months <- data.frame(month = matches, ms_played=data_table$ms_played)
  # print(head(activity_hours))
  new_table <- activity_months %>% group_by(month) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
  
  # Vervollständige die Stunden von 0 bis 23, falls welche fehlen
  complete_months <- data.frame(month = 1:12)  # Alle Stunden von 0 bis 23
  new_table <- complete(complete_months, month) %>%
    left_join(new_table, by = "month") %>%
    replace_na(list(total_ms_played = 0))  # Setze fehlende Stunden auf 0
  
  # Verschiebe die Stunden so, dass sie bei 3 Uhr beginnen
  print(new_table, n=12)
  # plot(new_table$hour, new_table$total_ms_played)
  #print(head(new_table))
  # print((new_table$total_ms_played))
  
  # Ersetze die Monatsnummern (1 bis 12) durch JFMAMJJASOND
  month_labels <- strsplit("JFMAMJJASOND", "")[[1]]
  
  barplot(
    height = new_table$total_ms_played,                # Die Höhe der Balken
    names.arg = month_labels,               # Stunden als Namen der X-Achse
    col = "skyblue",                             # Farbe der Balken
    main = "Nach Monaten sortierte Wiedergabe",  # Titel des Plots
    xlab = "Monat",                             # Beschriftung der X-Achse
    ylab = "Minuten gespielt",                               # Beschriftung der Y-Achse
    ylim = c(0, max(new_table$total_ms_played, na.rm = TRUE) + 1000)  # Y-Achse anpassen
  ) 
}
  
  
}
