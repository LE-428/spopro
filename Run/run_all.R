
# Verzeichnis mit R-Dateien angeben
{
  print(getwd())
  r_files_directory <- "C:/Users/Leander/Documents/Spotify"
  print(list.files(pattern = ".R$", recursive = FALSE, full.names = TRUE))
  
  # Schleife durch alle JSON-Dateien im Verzeichnis
  r_file_list <- list.files(path = r_files_directory, pattern = ".R$", full.names = TRUE)
  # print(r_file_list)
  for (r_file in r_file_list) {
    # print(r_file)
    source(r_file)
  }
  
  }
  