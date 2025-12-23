# Alle Funktionen in die aktuelle Sitzung laden



{
  # Arbeitsverzeichnis und Zielverzeichnis setzen
  print(getwd())
  # r_files_directory <- "C:/Users/Leander/Documents/Spotify"
  r_files_directory <- as.character(getwd())
  # r_files_directory <- "C:/Users/Leander/Documents/Spotify/Files"
  # r_files_directory <- as.character(paste0(getwd(), "/Functions"))
  
  # Alle .R-Dateien im Verzeichnis (und Unterverzeichnissen) auflisten
  r_file_list <- list.files(path = r_files_directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  # Namen der Dateien, die nicht geladen werden sollen
  excluded_files <- c(file.path(r_files_directory, "Run/run_all.R"), file.path(r_files_directory, "Run/run_all_nonrecursive.R"),
                      file.path(r_files_directory, "app.R"), file.path(r_files_directory, "getting_started.R"))
  
  # Schleife durch alle gefundenen R-Dateien
  for (r_file in r_file_list) {
    # Prüfen, ob die Datei zu den auszuschließenden gehört
    if (!r_file %in% excluded_files) {
      message("Lade: ", r_file)
      source(r_file)
    } else {
      message("Überspringe aktuelle Datei: ", r_file)
    }
  }
}

