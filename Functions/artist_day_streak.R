# Find the longest streaks of days with playback by a certain artist

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
    
    df$ts <- as.Date(substr(df$ts, 1, 10))  # Ensure that `ts` is interpreted as a date
    df <- as.data.table(df)  # Convert to data.table
    
    # Determine a streak ID for each artist
    df[, streak_id := cumsum(c(1, diff(ts) != 1)), by = Artist]
    
    # Aggregate streaks per artist
    streaks <- df[, .(
      start_date = min(ts),
      end_date = max(ts),
      Days = .N
    ), by = .(Artist, streak_id)]
    
    streaks <- unique(subset(streaks[order(-Days, Artist)], select = -c(streak_id)))
    
    # Independent computation of the "All" streak (without artist grouping)
    df_all <- unique(df[, .(ts)])  # Consider only unique days
    df_all[, streak_id := cumsum(c(1, diff(ts) != 1))]  # Compute streaks for all days
    
    all_streaks <- df_all[, .(
      start_date = min(ts),
      end_date = max(ts),
      Days = .N
    ), by = streak_id][order(-Days)]  # Compute the longest streak
    
    all_time_streak <- all_streaks[1]  # Select the longest streak
    all_time_streak[, Artist := "--Any artist--"]
    all_time_streak[, streak_id := NULL]
    
    streaks <- rbind(all_time_streak, streaks, fill = TRUE)
    
    # Change column order (Artist, Days, start_date, end_date)
    streaks <- streaks[, .(Artist, Days, start_date, end_date)]
    
    # Convert date values to strings for Shiny
    streaks$start_date <- as.character(streaks$start_date)
    streaks$end_date <- as.character(streaks$end_date)
    
    streaks <- as.data.frame(streaks)
    
    # Output
    print(head(streaks, n = top_x))
  }
}
