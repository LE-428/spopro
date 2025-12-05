# PDF erstellen, welches die Top 100 Lieder aller im Datensatz enthaltenden Jahre
# auflistet und Übereinstimmungen miteinander verbindet
{

{
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyr)
library(viridis)
library(MASS)
# library(hrbrthemes)
library(GGally)
}

top_tracks_over_time_plot <- function(data = all_data, save_plot = FALSE){
  
  data$ts <- ymd_hms(data$ts)
  
  # Extrahiere das Jahr und Monat aus dem Zeitstempel
  data$year <- year(data$ts)

  data <- subset(data, data$ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern
  
  # data <- data.frame(year = data$year, master_metadata_track_name = data$master_metadata_track_name, master_metadata_album_artist_name = data$master_metadata_album_artist_name)  
  data <- data[c('year', 'master_metadata_track_name', 'master_metadata_album_artist_name')]
  # colnames(data) <- c("year","track","artist")
  result <- split(data,data$year)
  # return(result)
  top <- list()
  
  # Iteriere über alle einzigartigen Jahre in den Daten
  unique_years <- sort(unique(data$year))
  
  for (i in unique_years) { # Jahresgrenzen evtl noch automatisch anpassend machen
  # for (i in min(unique(data$year)):max(unique(data$year))){ # Jahresgrenzen evtl noch automatisch anpassend machen
    a <- result[[as.character(i)]]
    b <- top_tracks(a, 100, filter_streams = FALSE)
    # print(i) # Welches Jahr verursacht das Problem?
    # print(b) # Wie sieht das Ergebnis aus?
    # print(nrow(b)) # Ist es NULL oder 0?
    b$id <- seq(1, nrow(b))
    # print(b, n=100)
    top[[as.character(i)]] <- b
  }
    
  combined_df <- bind_rows(top, .id = "Jahr")
  # print(head(combined_df))
  
  
  # Plot

  p <- ggplot(combined_df, aes(x = factor(Jahr),y = id,group = Track, color = Track)) +
    geom_line(aes(x = factor(Jahr), y = id)) +
    geom_point(shape = 16, size = 1) + 
    geom_text(aes(label = Track), vjust = -0.5, hjust = 0.5, size = 1) +  
    labs(title = paste0("Top 100 from ", min(unique(data$year))," to ", max(unique(data$year))), x = "Year", y = "Rank") +
    # theme_ipsum() +
    theme(legend.position="none",
      plot.title = element_text(hjust = 0.5),  # Center title
      plot.background = element_rect(fill = "white", color = "white"),  # Set background to white
  
    )
  print(p)
  
  suppressWarnings(
  # Exportiere den ggplot als hochauflösendes PDF
  if (save_plot == TRUE) {
    ggsave(
      filename = "top_tracks_plot.pdf",  # Dateiname
      plot = p,                         # Dein ggplot2-Objekt
      device = pdf,               # Verwende cairo für bessere Text-Darstellung
      width = 12,                       # Breite in Zoll
      height = 8,                       # Höhe in Zoll
      dpi = 300                         # Auflösung (dots per inch)
    )
  }
  )
  
  
  # return(combined_df)
}

}
