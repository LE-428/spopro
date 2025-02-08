# Ctrl + '.' (Punkt) für schnelle Suche von files/functions
# Gibt die am häufigsten auftretenden Jahre bei den release_dates aus,
# benötigt erweiterte Tabelle (mit API)

{


release_years_plot <- function(data_table_ext){
    # Sicherstellen, dass die release_date-Spalte als Zeichenkette vorliegt
    data_table_ext$release_date <- as.character(data_table_ext$release_date)
    
    # Daten filtern, basierend auf ms_played
    data_table_ext <- subset(data_table_ext, ms_played > 30000)
    
    # Extrahiere das Jahr aus dem Datumsstring (YYYY-MM-DD)
    matches <- as.integer(sub("^([0-9]{4}).*", "\\1", data_table_ext$release_date))
    
    # Häufigste Jahre berechnen
    year_counts <- table(matches)
  
    
    # Prozentsatz berechnen: Häufigkeit jedes Jahres / Gesamtzahl der Zeilen * 100
    total_count <- sum(year_counts)  # Gesamtzahl der Zeilen
    year_percentages <- (year_counts / total_count) * 100
    
    # Ausgabe der prozentualen Anteile
    print(year_percentages)
    
    # Barplot der prozentualen Anteile der Jahre
    barplot(
      height = year_percentages,                # Prozentsätze als Höhe der Balken
      names.arg = names(year_percentages),      # Jahreszahlen als Namen der X-Achse
      col = "steelblue1",                          # Farbe der Balken
      main = "Percentage distribution of release years",  # Titel des Plots
      xlab = "Year",                            # Beschriftung der X-Achse
      ylab = "Percentage (%)",         # Beschriftung der Y-Achse
      ylim = c(0, max(year_percentages) + 10)   # Y-Achse anpassen
    ) 
}
    
}




