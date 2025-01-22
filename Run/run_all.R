# Alle Funktionen in die aktuelle Sitzung laden



{
# Arbeitsverzeichnis und Zielverzeichnis setzen
print(getwd())
#r_files_directory <- "C:/Users/Leander/Documents/Spotify"
r_files_directory <- as.character(getwd())

# Alle .R-Dateien im Verzeichnis (und Unterverzeichnissen) auflisten
r_file_list <- list.files(path = r_files_directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

# Namen der Datei, die nicht geladen werden soll
excluded_file <- file.path(r_files_directory, "Run/run_all.R")

# Schleife durch alle gefundenen R-Dateien
for (r_file in r_file_list) {
  # Prüfen, ob es sich um die auszuschließende Datei handelt
  if (r_file != excluded_file) {
    message("Lade: ", r_file)
    source(r_file)
  } else {
    message("Überspringe aktuelle Datei: ", r_file)
  }
}
  

}
