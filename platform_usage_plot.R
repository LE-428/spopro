# Show the platforms which wered used to listen to music

{
  library(ggplot2)
  library(dplyr)
  library(ggrepel)


platform_usage_plot <- function(data_frame) {
  # Definiere die interessierenden Plattformen
  platforms <- c("osx", "windows", "ios", "android")
  
  # Konvertiere die Platform-Spalte in Kleinbuchstaben, um die Suche case-insensitive zu machen
  plat_lower <- tolower(data_frame$platform)
  
  # Zähle, wie oft jeder Begriff in der Spalte vorkommt
  counts <- sapply(platforms, function(p) sum(grepl(p, plat_lower)))
  
  # Zähle "Other" (alles, was nicht in platforms ist)
  counts["other"] <- sum(!grepl(paste(platforms, collapse = "|"), plat_lower))
  
  # Berechne den Gesamtwert der Einträge
  total_count <- sum(counts)
  
  # Berechne die Prozentwerte
  percent_counts <- (counts / total_count) * 100
  
  # Erstelle DataFrame für ggplot
  df <- data.frame(
    platform = names(percent_counts),
    percent = percent_counts
  )
  
  # Berechne die mittleren Positionen der Segmente (für die Label)
  df$ymax <- cumsum(df$percent)
  df$ymin <- c(0, head(df$ymax, n = -1))
  df$midpoint <- (df$ymin + df$ymax) / 2
  df$Label <- paste0(df$platform, " (", round(df$percent, 1), "%)")
  
  # Erstelle das Kreisdiagramm mit Labels außerhalb des Kuchens
  ggplot(df, aes(x = "", y = percent, fill = platform)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +  # Umkreisrichtung
    theme_void() +  # Hintergrund entfernen
    labs(title = "Platform Usage Share") +
    
    # Verbindungslinien zu den Labels
    # geom_segment(aes(x = 1.1, xend = 1.5, y = midpoint, yend = midpoint), 
    #              color = "black", size = 0.5) +
    
    # Labels außerhalb des Kuchens mit `geom_label_repel`
    geom_label_repel(aes(x = 1.7, y = midpoint, label = Label), 
                     size = 4, color = "black", nudge_x = 0.1, box.padding = 0.35, segment.size = 0.5) +
    
    theme(legend.position = "none",  # Legende entfernen
          plot.title = element_text(hjust = 0.5))  # Titel zentrieren
  
  # Optional: Rückgabe der Prozentwerte
  # return(percent_counts)
}


}
