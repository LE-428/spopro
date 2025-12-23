# Output the most popular artists ordered by month

# Example:

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
      group_by(artist_name) %>%                   # Group by artist name
      summarise(total_ms_played = round(sum(ms_played) / 1000 / 60)) %>%  # Calculate sum of ms_played
      arrange(desc(total_ms_played)) %>%          # Sort in descending order
      slice(1)                                    # Select the artist with the highest total
  }
  
  artist_month <- function(data_table) {
    # print(head(data_table))
    # Filter out streams under 30s, select only 3 columns
    data_table <-
      subset(
        data_table,
        ms_played > 30000 &                        # Filter streams shorter than 30s; Spotify counts streams after 30s
          !is.na(master_metadata_track_name),
        select = c(ts, ms_played, master_metadata_album_artist_name)
      )
    #print(head(data_table))
    
    # Extract month from the timestamp
    matches <-
      as.integer(regmatches(
        data_table$ts,
        regexpr("(?<=\\d{4}-)\\d{2}", data_table$ts, perl = TRUE)
      ))
    #print(length(matches))
    #print(length(data_table$ts))
    # print(head(matches))
    
    # Create new table
    activity_months <-
      data.frame(
        month = matches,
        ms_played = data_table$ms_played,
        artist_name = data_table$master_metadata_album_artist_name
      )
    #print(head(activity_months))
    
    # Group by month and apply the function
    top_artists_by_month <- activity_months %>%
      group_by(month) %>%                     # Group by month
      group_split() %>%                       # Split into groups
      lapply(artist_time)
    #print(top_artists_by_month)
    
    # Combine results
    result <- bind_rows(top_artists_by_month, .id = "group")
    colnames(result) <- c("Month", "Artist", "Minutes")
    result$Minutes <- as.integer(result$Minutes)
    print(result)
  }
}
