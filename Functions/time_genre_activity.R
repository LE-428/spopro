# Find the most listened-to genre depending on time of day

# input: data_table_ext


# > time_genre_activity(all_mo_ext)
# # A tibble: 23 × 3
# hour top_genre           top_genre_freq
# <int> <chr>                        <int>
#   1     0 east coast hip hop              38
# 2     1 bass house                      10
# 3     3 alternative hip hop              2
# 4     4 breakcore                        1
# 5     5 alternative r&b                  1
# 6     6 hip hop                          3
# 7     7 german hip hop                  33
# 8     8 german hip hop                  80
# 9     9 german hip hop                 124
# 10    10 german hip hop                 217
# 11    11 german hip hop                 177
# 12    12 german hip hop                 '126
# [...]



time_genre_activity <- function(data_table_ext){
  
  data_table_ext <- subset(data_table_ext, ms_played > 30000 & conn_country == "DE" & artist_genres != "",
                           select=c(ts, artist_genres, ms_played)) # Filter out streams shorter than 30s; Spotify counts a stream after 30s
  #print(head(data_table_ext, n = 50))
  #print(length(data_table_ext$artist_genres))
  
  # Extract hour
  matches <- as.integer(regmatches(data_table_ext$ts, regexpr("(?<=T)\\d{2}", data_table_ext$ts, perl = TRUE)))
  # print(head(matches))
  activity_hours <- data.frame(hour = matches, artist_genres = data_table_ext$artist_genres,
                               ms_played = data_table_ext$ms_played)
  #activity_hours$artist_genres <- strsplit(activity_hours$artist_genres, ",")
  #print(activity_hours$artist_genres)
  #print(head(activity_hours, n = 50))
  
  #genre_list <- strsplit(data_table_ext$artist_genres, ",")
  #print(head(genre_list, n = 100))
  #all_genres <- unlist(genre_list)
  #print(all_genres)
  
  #top_genre_by_hour <- activity_hours %>% group_by(hour) %>% group_split() %>% lapply(top_genres) %>% slice_head(n = 1)
  #print((top_genre_by_hour))
  
  # Function to determine the most popular genre per hour
  top_genre_per_hour <- activity_hours %>%
    group_by(hour) %>%
    # Call the function `top_genres` to determine the most frequent genre per hour
    summarise(top_genre = names(sort(table(unlist(strsplit(artist_genres, ","))), decreasing = TRUE)[1]),
              top_genre_freq = max(table(unlist(strsplit(artist_genres, ",")))))
  
  # Print the result
  print(top_genre_per_hour, n = 24)
}
