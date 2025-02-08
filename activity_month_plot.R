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
  
  
activity_month_plot <- function(data_table){
  # print(head(data_table))
  data_table <- subset(data_table, ms_played > 30000, select=c(ts, ms_played)) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  #print(head(data_table))
  # Beispiel: Teile des Strings extrahieren, die dem Muster entsprechen
  matches <- as.integer(regmatches(data_table$ts, regexpr("(?<=\\d{4}-)\\d{2}", data_table$ts, perl = TRUE)))
  # print(head(matches))
  activity_months <- data.frame(month = matches, ms_played=data_table$ms_played)
  # print(head(activity_hours))
  new_table <- activity_months %>% group_by(month) %>% summarise(total_ms_played = round(sum(ms_played, na.rm = TRUE) / 1000 / 60))
  
  # Die zwölf Monate eintragen
  complete_months <- data.frame(month = 1:12)  # alle Monate
  new_table <- complete(complete_months, month) %>%
    left_join(new_table, by = "month") %>%
    replace_na(list(total_ms_played = 0))  # Setze fehlende Monate auf 0
  
  print("Minutes played by month")
  print(new_table, n=12)
  # plot(new_table$hour, new_table$total_ms_played)
  #print(head(new_table))
  # print((new_table$total_ms_played))
  
  # Ersetze die Monatsnummern (1 bis 12) durch JFMAMJJASOND
  month_labels <- strsplit("JFMAMJJASOND", "")[[1]]
  
  barplot(
    height = new_table$total_ms_played,                # Die Höhe der Balken
    names.arg = month_labels,               # Stunden als Namen der X-Achse
    col = "orangered",                             # Farbe der Balken
    main = "Minutes played by month",  # Titel des Plots
    xlab = "Month",                             # Beschriftung der X-Achse
    ylab = "Minutes played",                               # Beschriftung der Y-Achse
    ylim = c(0, max(new_table$total_ms_played, na.rm = TRUE) + 1000)  # Y-Achse anpassen
  ) 
}
  
  
}
