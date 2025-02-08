# Funktion zum Setzen der Spotify API-Anmeldedaten als Umgebungsvariablen

set_api_creds <- function(client_id, client_secret) {
  # Überprüfen, ob die Eingaben nicht leer sind
  if (missing(client_id) || missing(client_secret) || client_id == "" || client_secret == "") {
    stop("Both Client ID and Client Secret are required.")
  }
  
  # Setze die Systemvariablen für die Spotify API-Anmeldedaten
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
  
  message("Spotify API credentials have been set.")
}
