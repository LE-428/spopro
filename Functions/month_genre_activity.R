# Find the most listened-to genre per month

# Requires an extended dataset with API data


# > month_genre_activity(all_mo_ext)
# # A tibble: 12 × 3
# month top_genre      top_genre_freq
# <int> <chr>                   <int>
#   1     1 german hip hop            670
# 2     2 jazz rap                  207
# 3     3 german hip hop            308
# 4     4 german hip hop            280
# 5     5 german hip hop            100
# 6     6 neo soul                   68
# 7     7 german hip hop            232
# 8     8 german hip hop            214
# 9     9 rap                       104
# 10    10 future bass               157
# 11    11 rap                        79
# 12    12 german hip hop             83

month_genre_activity <- function(data_table_ext) {
  data_table_ext <-
    subset(
      data_table_ext,
      ms_played > 30000 & conn_country == "DE" & artist_genres != "",
      select = c(ts, artist_genres, ms_played)
    ) # Filter out streams shorter than 30s, since Spotify counts a stream after 30s
  #print(head(data_table_ext, n = 50))
  #print(length(data_table_ext$artist_genres))
  # Extract month
  matches <-
    as.integer(regmatches(
      data_table_ext$ts,
      regexpr("(?<=\\d{4}-)\\d{2}", data_table_ext$ts, perl = TRUE)
    ))
  # print(head(matches))
  activity_months <-
    data.frame(
      month = matches,
      artist_genres = data_table_ext$artist_genres,
      ms_played = data_table_ext$ms_played
    )
  #activity_months$artist_genres <- strsplit(activity_months$artist_genres, ",")
  #print(activity_months$artist_genres)
  #print(head(activity_months, n = 50))
  
  #genre_list <- strsplit(data_table_ext$artist_genres, ",")
  #print(head(genre_list, n = 100))
  #all_genres <- unlist(genre_list)
  #print(all_genres)
  
  
  #top_genre_by_month <- activity_months %>% group_by(month) %>% group_split() %>% lapply(top_genres) %>% slice_head(n = 1)
  #print((top_genre_by_month))
  
  # Function to determine the most popular genre per month
  top_genre_per_month <- activity_months %>%
    group_by(month) %>%
    # Call the function `top_genres` to determine the most frequent genre per month
    summarise(top_genre = names(sort(table(
      unlist(strsplit(artist_genres, ","))
    ), decreasing = TRUE)[1]),
    top_genre_freq = max(table(unlist(
      strsplit(artist_genres, ",")
    )))) %>%
    rename(Month = month,
           Top_Genre = top_genre,
           Frequency = top_genre_freq)
  
  # Print the result
  print(top_genre_per_month, n = 12)
}
