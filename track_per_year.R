# Welches Lied hast du an einem Tag am häufigsten gehört, welcher Tag und wie oft?
# Input: data_table - Datentabelle
#        top_x      - Anzahl der ausgegeben Tage

# Beispiel

# > track_per_year(y2020, 5)
#     date       track                                          plays
# 7   2020-01-07                                     Foolery    40
# 119 2020-04-30 R.I.P. Fredo (feat. Young Nudy) - Notice Me    35
# 174 2020-07-03                                 High School    34
# 63  2020-03-04                        Welcome To The Party    32
# 185 2020-07-14                               Broken Hearts    32

track_per_year <- function(data_table = all_data, top_x){
  data_table <- subset(data_table, ms_played > 30000) # Streams mit weniger als 30s Dauer rausfiltern, für Spotify zählt ein stream ebenfalls nach 30s
  
  days_table <- data.frame(ts = substr(data_table$ts, 1, 10))
  
  häufigkeiten_days <- table(days_table$ts) # Um schnelle Ergebnisse zu bekommen werden nur die Artists mit vielen Streams berücktsichtigt
  sortierte_häufigkeiten <- sort(häufigkeiten_days, decreasing = TRUE)
  x_häufigsten_days <- head(sortierte_häufigkeiten, 20 * top_x)
  days_table_unique <- data.frame(ts = attr(x_häufigsten_days, "names"))
  
  # aux_table <- data_table[(which(grepl(paste0("^", track_string, "$"), data_table$master_metadata_track_name, ignore.case=TRUE))),]
  
  # # days_table_unique <- unique(days_table)
  top_tracks_all_days <- data.frame(date = days_table_unique$ts)
  for(i in 1:length(days_table_unique$ts)){
    logic <- grep(days_table_unique$ts[i], days_table$ts)
    # days_table_unique$sum[i] <- sum(data_table$ms_played[logic])
    one_day_table <- data.frame(track = data_table$master_metadata_track_name[logic])
    häufigkeiten_track_day <- sort(table(one_day_table$track), decreasing = TRUE)
    # sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
    hottest_track <- head(häufigkeiten_track_day, 1)
    top_tracks_all_days$track[i] = attr(hottest_track, "names");
    top_tracks_all_days$plays[i] = hottest_track
  }
  top_tracks_all_days <- top_tracks_all_days[order(-top_tracks_all_days$plays), ]
  return(head(top_tracks_all_days, top_x))
}
