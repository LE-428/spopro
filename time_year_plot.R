# Plot the listening time over the years contained in the dataset

time_year_plot <- function(data_frame){
  all_years <- sort(unique(substr(data_frame$ts, 1, 4)), decreasing = FALSE)
  
  listening_times <- numeric(length(all_years))  # Vektor für Hörzeiten initialisieren
  
  for (i in seq_along(all_years)) {
    year <- all_years[i]
    listening_times[i] <- listening_time(extract_year(year, data_frame))  # Hörzeit für jedes Jahr speichern
  }
  
  # Barplot mit Jahren auf x-Achse und Minuten auf y-Achse
  barplot(listening_times, names.arg = all_years, col = "palegreen",
          main = "Listening Time per Year",
          xlab = "Year", ylab = "Minutes")
}
