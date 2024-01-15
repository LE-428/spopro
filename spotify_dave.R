{
install.packages("jsonlite")
install.packages("dplyr")
}

{
library(dplyr)
library(jsonlite)
}

# Verzeichnis mit JSON-Dateien angeben
{
  json_files_directory <- "C:/Users/Leander/Documents/Spotify/DaveDaten/my_spotify_data/Spotify Extended Streaming History"
  
  # Liste für geladene Daten erstellen
  loaded_data_list <- list()
  
  # Schleife durch alle JSON-Dateien im Verzeichnis
  json_file_list <- list.files(path = json_files_directory, pattern = "\\.json$", full.names = TRUE)
  for (json_file in json_file_list) {
    loaded_data <- fromJSON(json_file)
    loaded_data_list[[json_file]] <- loaded_data
  }
  
  all_data = bind_rows(loaded_data_list)
}

# {
#   spotify_data1 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2014-2019_0.json")
#   spotify_data2 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2019-2020_1.json")
#   spotify_data3 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2020-2021_2.json")
#   spotify_data4 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2021_3.json")
#   spotify_data5 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2021-2022_4.json")
#   spotify_data6 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2022-2023_5.json")
#   spotify_data7 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2023_6.json")
#   spotify_data8 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Audio_2023_7.json")
#   spotify_data9 = fromJSON("DaveDaten\\my_spotify_data\\Spotify Extended Streaming History\\Streaming_History_Video_2022-2023.json")
#   # DaveDaten\my_spotify_data\Spotify Extended Streaming History
#   
#   all_data <- bind_rows(spotify_data1, spotify_data2, spotify_data3, spotify_data4, spotify_data5, spotify_data6, spotify_data7, spotify_data8, spotify_data9)
# }

{
  y2014 <- extract_year("2014", all_data)
  y2015 <- extract_year("2015", all_data)
  y2016 <- extract_year("2016", all_data)
  y2017 <- extract_year("2017", all_data)
  y2018 <- extract_year("2018", all_data)
  y2019 <- extract_year("2019", all_data)
  y2020 <- extract_year("2020", all_data)
  y2021 <- extract_year("2021", all_data)
  y2022 <- extract_year("2022", all_data)
  y2023 <- extract_year("2023", all_data)
}

all_dave = all_data

# Noch zu erledigen:
# Top Tage (meiste Musik gehört) - ERLEDIGT, top_days_time
# An einem Tag Lied am häufigsten gespielt, welcher Tag und wie oft - ERLEDIGT, track_per_year
# Top Albums, Top Artists nach Wiedergabezeit

