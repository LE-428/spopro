{
  # Arbeitsverzeichnis und Zielverzeichnis setzen
  print(getwd())
  # r_files_directory <- "C:/Users/Leander/Documents/Spotify"
  r_files_directory <- as.character(getwd())
  
  # Alle .R-Dateien im Verzeichnis (und Unterverzeichnissen) auflisten
  r_file_list <- list.files(path = r_files_directory, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  
  # Namen der Dateien und Ordner, die nicht geladen werden sollen
  excluded_files <- c(file.path(r_files_directory, "app.R"))
  
  # Schleife durch alle gefundenen R-Dateien
  for (r_file in r_file_list) {
    # Prüfen, ob die Datei zu den auszuschließenden gehört (einschließlich des Ordners)
    if (!r_file %in% excluded_files) {
      message("Lade: ", r_file)
      source(r_file)
    } else {
      message("Überspringe aktuelle Datei: ", r_file)
    }
  }
}
