# Quick overview over the dataset

{
library(jsonlite)

quick_stats <- function(data_table){
  country_dict <- fromJSON("market_to_country.json")
  # print(country_dict)
  
  num_tracks <- length(unique(data_table$spotify_track_uri))
  num_artists <- length(unique(data_table$master_metadata_album_artist_name))
  
  countries <- sort(table(data_table$conn_country), decreasing = TRUE)
  num_countries <- length(countries)
  
  timestamps <- subset(data_table, select = c(ts))
  years <- substr(timestamps$ts, 1, 4)
  unique_years <- unique(years)
  
  print(paste("Years found in the dataset: ", min(unique_years), "-", max(unique_years)))
  print(paste("Number of different tracks: ", num_tracks))
  print(paste("Number of different artists: ", num_artists))
  print(paste("Number of countries that you listened music in: ", num_countries))
  print("Most common countries: ")
  result <- data.frame(
    Country = unlist(country_dict[names(head(countries))]),
    Streams = as.integer(head(countries))
  )
  return(invisible(result))
}

}
