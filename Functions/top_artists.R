# Return top artists by minutes played of inserted dataframe
# data_table: table with raw data, top_x: integer value, the top x entries will be returned

# Example

# y2017 <- extract_year("2017", all_data)
# > top_artists(y2017, 5)
#         artist             playtime
# 679               Sa4      768
# 1               Drake      705
# 363 187 Strassenbande      629
# 342              Gzuz      554
# 208        RAF Camora      324

top_artists <- function(df, top_x = 5) {
  df %>%
    filter(!is.na(master_metadata_album_artist_name)) %>%             # Remove NA values
    group_by(master_metadata_album_artist_name) %>%                   # Group by artist name
    summarise(
      Playtime = as.integer(round(sum(ms_played) / 1000 / 60))        # Compute total duration in minutes
    ) %>%
    arrange(desc(Playtime)) %>%                           # Sort descending by total duration
    slice_head(n = top_x) %>%
    rename(
      Artist = master_metadata_album_artist_name                # Rename columns
    ) %>%
    # Select top-x entries
    as.data.frame()                                                   # Return result as DataFrame
}
