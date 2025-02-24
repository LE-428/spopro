# Show the platforms which wered used to listen to music

platform_usage_plot <- function(data_table) {
  # Definiere die interessierenden Plattformen
  platforms <- c("osx", "windows", "ios", "android")
  
  # Konvertiere die Platform-Spalte in Kleinbuchstaben, um die Suche case-insensitive zu machen
  plat_lower <- tolower(data_table$platform)
  
  # Zähle, wie oft jeder Begriff in der Spalte vorkommt
  counts <- sapply(platforms, function(p) sum(grepl(p, plat_lower)))
  
  # Zähle "Other" (alles, was nicht in platforms ist)
  counts["other"] <- sum(!grepl(paste(platforms, collapse = "|"), plat_lower))
  
  # Berechne den Gesamtwert der Einträge
  total_count <- sum(counts)
  
  # Berechne die Prozentwerte
  percent_counts <- (counts / total_count) * 100
  
  # Erstelle einen Barplot der prozentualen Ergebnisse
  barplot(percent_counts,
          main = "Platform usage share",
          xlab = "Platform",
          ylab = "Percent",
          col = "seagreen",
          ylim = c(0, 100))  # Setzt das y-Limit auf 0-100%, um eine klare Prozentanzeige zu ermöglichen
  
  # Optional: Rückgabe der Prozentwerte
  return(percent_counts)
}
