# Den Token für die Abfrage der Daten über die API aktualisieren


refresh_token <- function(){
  access_token <<- get_spotify_access_token()
  return(access_token)
}
