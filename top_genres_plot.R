# Pie-Plot with the 10 most popular genres

{
  
library(ggplot2)
library(dplyr)
library(ggrepel)

top_genres_plot <- function(data_table_ext, top_x = 10){
  result <- top_genres(data_table_ext, top_x = top_x)
  # Gesamtanzahl berechnen
  total <- sum(result$Count)
  
  # Reihenfolge umkehren, damit die Labels im richtigen Umlaufsinn angeordnet sind
  result <- result %>%
    arrange(desc(Count)) %>%
    mutate(Percentage = Count / total * 100,
           Label = paste0(Genre, " (", round(Percentage, 1), "%)"),
           Cumulative = rev(cumsum(rev(Count))),  # Reihenfolge umkehren
           Midpoint = Cumulative - Count / 2)  # Mittelpunkt jedes Segments
  
  # Kreisdiagramm mit korrektem Umlaufsinn
  ggplot(result, aes(x = "", y = Count, fill = Genre)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0, direction = -1) +  # Richtung umkehren!
    theme_void() +  # Hintergrund entfernen
    labs(title = "Top Genres Pie Chart") +
    
    # Verbindungslinien zu den Labels
    geom_segment(aes(x = 1.3, xend = 1.7, y = Midpoint, yend = Midpoint), 
                 color = "black", size = 0.5) +
    
    # Labels außerhalb des Kuchens
    geom_label_repel(aes(x = 2, y = Midpoint, label = Label), 
              size = 4, color = "black", hjust = 0.5) +
    
    theme(legend.position = "none",  # Legende entfernen, da Labels schon da sind
      plot.title = element_text(hjust = 0.5))
}

}
