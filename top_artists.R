# data_table Tabelle mit Rohdaten, top_x int Wert, ausgegeben werden die Top x Werte

# Details

# > top_artists(y2017, 5)
#         artist             playtime
# 679               Sa4      768
# 1               Drake      705
# 363 187 Strassenbande      629
# 342              Gzuz      554
# 208        RAF Camora      324

top_artists <- function(df, top_x = 5) {
  df %>%
    filter(!is.na(master_metadata_album_artist_name)) %>%             # NA-Werte entfernen
    group_by(master_metadata_album_artist_name) %>%                   # Gruppierung nach Künstlernamen
    summarise(
      Playtime = as.integer(round(sum(ms_played) / 1000 / 60))        # Gesamtdauer in Minuten berechnen
    ) %>%
    arrange(desc(Playtime)) %>%                           # Absteigend nach Gesamtdauer sortieren
    slice_head(n = top_x) %>%
    rename(
      Artist = master_metadata_album_artist_name                # Spalten umbenennen
    ) %>%
                                                                      # Top-x Einträge auswählen
    as.data.frame()                                                   # Ergebnis als DataFrame ausgeben
}
