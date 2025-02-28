# Welches Lied hast du an einem Tag am häufigsten gehört, welcher Tag und wie oft?
# Input: data_frame - Datentabelle
#        top_x      - Anzahl der ausgegeben Tage

# Beispiel

# > track_per_year(y2020, 5)
#     date       track                                          plays
# 7   2020-01-07                                     Foolery    40
# 119 2020-04-30 R.I.P. Fredo (feat. Young Nudy) - Notice Me    35
# 174 2020-07-03                                 High School    34
# 63  2020-03-04                        Welcome To The Party    32
# 185 2020-07-14                               Broken Hearts    32


track_per_year <- function(data_frame, top_x = 5){
  # Drop the podcast rows
  data_frame <- drop_podcasts(data_frame)
  # Drop streams with less than 30 seconds
  data_frame <- subset(data_frame, ms_played > 30000, select = c(ts, master_metadata_track_name, master_metadata_album_artist_name)) %>% 
    rename(
      Track = master_metadata_track_name,
      Artist = master_metadata_album_artist_name
    )
  data_frame$ts <- substr(data_frame$ts, 1, 10)
  # print(head(data_frame))
  
  data_frame <- data_frame %>% 
    # Group by day
    group_by(ts) %>% 
    # Get most popular track
    count(Track, Artist, sort = TRUE) %>%
    slice_head(n = 1) %>%  # Nimm den Künstler mit den meisten Zeilen pro Genre
    rename(
      Plays = n,
      Date = ts
    )  %>% 
    # Sort
    ungroup() %>% 
    arrange(desc(Plays)) %>% 
    slice_head(n = top_x)
  return(data_frame)
}