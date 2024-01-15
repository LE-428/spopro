{
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyr)
library(viridis)
library(MASS)
library(hrbrthemes)
library(GGally)
}

toptracks_over_time <- function(data = all_data){
  
  data$ts <- ymd_hms(data$ts)
  
  # Extrahiere das Jahr und Monat aus dem Zeitstempel
  data$year <- year(data$ts)

  
  
  # data <- data.frame(year = data$year, master_metadata_track_name = data$master_metadata_track_name, master_metadata_album_artist_name = data$master_metadata_album_artist_name)  
  data <- data %>%
      select(year, master_metadata_track_name, master_metadata_album_artist_name)
  # colnames(data) <- c("year","track","artist")
  result <- split(data,data$year)
  # return(result)
  top <- list()
  
  for (i in 2013:2023) {# Jahresgrenzen evtl noch automatisch anpassend machen
  # for (i in min(unique(data$year)):max(unique(data$year))){ # Jahresgrenzen evtl noch automatisch anpassend machen
    a <- result[[as.character(i)]]
    b <- top_tracks_mo(a,100)
    b$id <- seq(1, nrow(b))
    top[[as.character(i)]] <- b
  }
    
  combined_df <- bind_rows(top, .id = "Jahr")
  
  
  
  # Plot
  p <- ggplot(combined_df, aes(x = factor(Jahr),y = id,group = Track, color = Track)) +
    geom_line(aes(x = factor(Jahr), y = id)) +
    geom_point(shape = 16, size = 1) + 
    # geom_text(aes(label = Track), vjust = -0.5, hjust = 0.5, size = 3) +  
    labs(title = paste0("Top 100 von ", min(unique(data$year))," bis ", max(unique(data$year))), x = "Variable", y = "Value") +
    theme_ipsum() +
    theme(legend.position="none")
    
  print(p)
  
  # return(combined_df)
}
