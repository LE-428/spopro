{
  install.packages("jsonlite")
  install.packages("dplyr")
  
}

{
  library(dplyr)
  library(jsonlite)
  # library(tidyverse)
}

# nm <- list.files(path="MoritzDaten\\my_spotify_data_ex\\Spotify Extended Streaming History")
# # setwd(r"(paste(getwd(), "MoritzDaten\\my_spotify_data_ex\\Spotify Extended Streaming History"))")
# for (i in nm) {
#   temp_table <- fromJSON(r"(i)") # r() erzeugt einen kompatiblen Pfad / zu \\
#   # all_data <- bind_rows(all_data, temp_table)
# }


# Verzeichnis mit JSON-Dateien angeben
{
json_files_directory <- "C:/Users/Leander/Documents/Spotify/MoritzDaten/my_spotify_data_ex/Spotify Extended Streaming History"

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

all_mo = all_data

# Noch zu erledigen:
# Top Tage (meiste Musik gehört) - ERLEDIGT, top_days_time
# An einem Tag Lied am häufigsten gespielt, welcher Tag und wie oft - ERLEDIGT, track_per_year
# Top Albums, Top Artists nach Wiedergabezeit

