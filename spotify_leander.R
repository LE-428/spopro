{
install.packages("jsonlite")
install.packages("dplyr")
}


{
library(jsonlite)
library(dplyr)
}

# Verzeichnis mit JSON-Dateien angeben
{
  json_files_directory <- "C:/Users/Leander/Documents/Spotify/LeanderDaten"
  
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
#   spotify_data1 = fromJSON("endsong_0.json")
#   spotify_data2 = fromJSON("endsong_1.json")
#   spotify_data3 = fromJSON("endsong_2.json")
#   
#   all_data <- bind_rows(spotify_data1, spotify_data2, spotify_data3)
# }

{
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

all_leander = all_data

rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])


# Noch zu erledigen:
# Top Tage (meiste Musik gehört) - ERLEDIGT, top_days_time
# An einem Tag Lied am häufigsten gespielt, welcher Tag und wie oft - ERLEDIGT, track_per_year
# Top Albums, Top Artists nach Wiedergabezeit
# Top Lieder aus einem Album 
# Top Künstler nach Monat
# Zusätzliche Eingabe von Artist bei Track-Suche
