# Find the longest streaks of days with playback by certain artist

# > artist_days_streak(all_le, top_x = 6)
# Artist Days start_date   end_date
# 1 --Any artist--  183 2020-10-02 2021-04-02
# 2       Kid Cudi    5 2023-11-07 2023-11-11
# 3      21 Savage    4 2020-10-06 2020-10-09
# 4       A$AP Mob    4 2022-02-23 2022-02-26
# 5          Bcalm    4 2022-01-11 2022-01-14
# 6      Bertholet    4 2020-12-01 2020-12-04

{
  library(data.table)
  library(dplyr)
  
  artist_days_streak <- function(df, top_x = 10) {
    df <- drop_podcasts(df)
    df <- subset(df, ms_played > 30000, select = c(ts, master_metadata_album_artist_name)) %>% 
      rename(Artist = master_metadata_album_artist_name)
    
    df$ts <- as.Date(substr(df$ts, 1, 10))  # Stelle sicher, dass `ts` als Datum interpretiert wird
    df <- as.data.table(df)  # Umwandlung zu data.table
    
    # Streak-ID für jeden Künstler bestimmen
    df[, streak_id := cumsum(c(1, diff(ts) != 1)), by = Artist]
    
    # Streaks zusammenfassen pro Artist
    streaks <- df[, .(
      start_date = min(ts),
      end_date = max(ts),
      Days = .N
    ), by = .(Artist, streak_id)]
    
    streaks <- unique(subset(streaks[order(-Days, Artist)], select = -c(streak_id)))
    
    # Unabhängige Berechnung der "All"-Streak (ohne Artist-Gruppierung)
    df_all <- unique(df[, .(ts)])  # Nur einzigartige Tage betrachten
    df_all[, streak_id := cumsum(c(1, diff(ts) != 1))]  # Streaks für alle Tage berechnen
    
    all_streaks <- df_all[, .(
      start_date = min(ts),
      end_date = max(ts),
      Days = .N
    ), by = streak_id][order(-Days)]  # Längste Streak berechnen
    
    all_time_streak <- all_streaks[1]  # Längste Streak auswählen
    all_time_streak[, Artist := "--Any artist--"]
    all_time_streak[, streak_id := NULL]
    
    streaks <- rbind(all_time_streak, streaks, fill = TRUE)
    
    # Spaltenreihenfolge ändern (Artist, Days, start_date, end_date)
    streaks <- streaks[, .(Artist, Days, start_date, end_date)]
    
    # Datumswerte in Strings umwandeln für Shiny
    streaks$start_date <- as.character(streaks$start_date)
    streaks$end_date <- as.character(streaks$end_date)
    
    streaks <- as.data.frame(streaks)
    
    # Ausgabe
    print(head(streaks, n = top_x))
  }
}

