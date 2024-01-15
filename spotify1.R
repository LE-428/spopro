install.packages("jsonlite")
library(jsonlite)

install.packages("dplyr")
library(dplyr)

{
spotify_data1 = fromJSON("endsong_0.json")
spotify_data2 = fromJSON("endsong_1.json")
spotify_data3 = fromJSON("endsong_2.json")

all_data <- bind_rows(spotify_data1, spotify_data2, spotify_data3)
}

häufigkeiten_album <- table(all_data$master_metadata_album_album_name)
häufigster_wert_album <- names(häufigkeiten_album)[which.max(häufigkeiten_album)]

häufigkeiten_artist <- table(all_data$master_metadata_album_artist_name)
häufigster_wert_artist <- names(häufigkeiten_artist)[which.max(häufigkeiten_artist)]

häufigkeiten_track <- table(all_data$master_metadata_track_name)
häufigster_wert_track <- names(häufigkeiten_track)[which.max(häufigkeiten_track)]

sortierte_häufigkeiten <- sort(häufigkeiten_artist, decreasing = TRUE)
zehn_häufigsten <- head(sortierte_häufigkeiten, 30)
cat("Die zehn häufigsten Werte sind:\n")
print(zehn_häufigsten)

sortierte_häufigkeiten <- sort(häufigkeiten_album, decreasing = TRUE)
dreissig_häufigsten_alben <- head(sortierte_häufigkeiten, 30)
cat("Die dreißig häufigsten Werte sind:\n")
print(dreissig_häufigsten_alben)

{
sortierte_häufigkeiten <- sort(häufigkeiten_track, decreasing = TRUE)
dreissig_häufigsten_track <- head(sortierte_häufigkeiten, 50)
cat("Die dreißig häufigsten Werte sind:\n")
print(dreissig_häufigsten_track)
}

# year_values = which(grepl("2015", all_data$ts))
# data_2015 <- all_data[(year_values),]
year_values = which(grepl("2022", all_data$ts))
data_2022 <- all_data[(year_values),]
data_2019 <- all_data[(which(grepl("2019", all_data$ts))),]

häufigkeiten_artist_2022 <- table(data_2022$master_metadata_album_artist_name)
häufigster_wert_2022 <- names(häufigkeiten_artist_2022)[which.max(häufigkeiten_artist_2022)]

häufigkeiten_artist_2019 <- table(data_2019$master_metadata_album_artist_name)
häufigster_wert_2019 <- names(häufigkeiten_artist_2019)[which.max(häufigkeiten_artist_2019)]

{
  sortierte_häufigkeiten_2022 <- sort(häufigkeiten_artist_2022, decreasing = TRUE)
zehn_häufigsten_2022 <- head(sortierte_häufigkeiten_2022, 30)
cat("Die zehn häufigsten Werte sind:\n")
print(zehn_häufigsten_2022)
}

{
  sortierte_häufigkeiten_2019 <- sort(häufigkeiten_artist_2019, decreasing = TRUE)
  zehn_häufigsten_2019 <- head(sortierte_häufigkeiten_2019, 30)
  cat("Die zehn häufigsten Werte sind:\n")
  print(zehn_häufigsten_2019)
}

rm(all_data)