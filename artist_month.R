# Die beliebtesten Künstler nach Monat geordnet ausgeben

# Beispiel:

# > artist_month(extract_year(2024, all_le))
# # A tibble: 12 × 3
# month artist_name          minutes
# <chr> <chr>                  <dbl>
#   1 1     Kendrick Lamar           220
# 2 2     Don Toliver              134
# 3 3     Anderson .Paak           108
# 4 4     Isaiah Rashad            125
# 5 5     Ludwig van Beethoven     121
# 6 6     Fleetwood Mac            217
# 7 7     MIKE DEAN                174
# 8 8     The Weeknd                92
# 9 9     The Weeknd               104
# 10 10    Ufo361                    50
# 11 11    The Weeknd                31
# 12 12    Ken Carson                 2

{
  {
    library(dplyr)
    library(roxygen2)
  }
  
  
  artist_time <- function(df) {
    df %>%
      group_by(artist_name) %>%                   # Gruppierung nach Künstlernamen
      summarise(total_ms_played = round(sum(ms_played) / 1000 / 60)) %>%  # Summe der ms_played berechnen
      arrange(desc(total_ms_played)) %>%          # Absteigend sortieren
      slice(1)                                    # Den Künstler mit der höchsten Summe auswählen
  }
  
  
  artist_month <- function(data_table) {
    # print(head(data_table))
    # Streams unter 30s aussortieren, nur 3 Spalten auswählen
    data_table <-
      subset(
        data_table,
        ms_played > 30000 &
          !is.na(spotify_track_uri),
        select = c(ts, ms_played, master_metadata_album_artist_name)
      ) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
    #print(head(data_table))
    
    
    # Monat extrahieren aus dem timestamp
    matches <-
      as.integer(regmatches(
        data_table$ts,
        regexpr("(?<=\\d{4}-)\\d{2}", data_table$ts, perl = TRUE)
      ))
    #print(length(matches))
    #print(length(data_table$ts))
    # print(head(matches))
    
    # Neue Tabelle erstellen
    activity_months <-
      data.frame(
        month = matches,
        ms_played = data_table$ms_played,
        artist_name = data_table$master_metadata_album_artist_name
      )
    #print(head(activity_months))
    
    # Gruppierung nach Monat und Anwendung der Funktion
    top_artists_by_month <- activity_months %>%
      group_by(month) %>%                     # Nach Monat gruppieren
      group_split() %>%                       # Unterteilen in Gruppen
      lapply(artist_time)
    #print(top_artists_by_month)
    
    # Ergebnis zusammenführen
    result <- bind_rows(top_artists_by_month, .id = "group")
    colnames(result) <- c("month", "artist_name", "minutes")
    print(result)
  }
  
  
  
}
