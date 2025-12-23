# Find the most played track within one day, return day and number of streams
# Input: data_frame - raw data table
#        top_x      - number of top days to return

# Example

# > track_per_year(y2020, 5)
#     date       track                                          plays
# 7   2020-01-07                                     Foolery    40
# 119 2020-04-30 R.I.P. Fredo (feat. Young Nudy) - Notice Me    35
# 174 2020-07-03                                 High School    34
# 63  2020-03-04                        Welcome To The Party    32
# 185 2020-07-14                               Broken Hearts    32

track_per_year <- function(data_frame, top_x = 5){
  # Remove podcast streams
  data_frame <- drop_podcasts(data_frame)
  
  # Keep only streams longer than 30 seconds and select relevant columns
  data_frame <- subset(data_frame, ms_played > 30000, 
                       select = c(ts, master_metadata_track_name, master_metadata_album_artist_name)) %>% 
    rename(
      Track = master_metadata_track_name,
      Artist = master_metadata_album_artist_name
    )
  
  # Extract date only (YYYY-MM-DD)
  data_frame$ts <- substr(data_frame$ts, 1, 10)
  
  # Group by day and find the most played track per day
  data_frame <- data_frame %>% 
    group_by(ts) %>% 
    count(Track, Artist, sort = TRUE) %>%        # Count occurrences of each track
    slice_head(n = 1) %>%                        # Take the most played track per day
    rename(
      Plays = n,
      Date = ts
    ) %>% 
    ungroup() %>% 
    arrange(desc(Plays)) %>%                     # Sort by number of plays descending
    slice_head(n = top_x)                        # Return only top_x days
  
  return(data_frame)
}
