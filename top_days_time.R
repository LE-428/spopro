# Ctrl + Shift +  C zum Auskommentieren

# An welchen n  Tagen habe ich am längsten Musik gehört?
# data_frame Datentabelle mit allen Spalten, top_n Anzahl der obersten Tage, die ausgegeben werden

# Beispiel:

# > top_days_time(y2022, 5)
#   date           minutes
# 1 2022-07-29     423
# 2 2022-02-01     350
# 3 2022-01-19     338
# 4 2022-01-21     337
# 5 2022-09-10     324


top_days_time <- function(data_frame, top_x = 5){
  days_table <- subset(data_frame, select = c(ts, ms_played))
  # Extract the date and drop the HH:MM:SS part
  days_table$ts <- substr(days_table$ts, 1, 10)
  
  days_table <- days_table %>% 
    # Group by the day
    group_by(ts) %>% 
    # Collapse the ms_played column
    summarise(
      Minutes = as.integer(sum(ms_played) / 60000)
    ) %>% 
    # Sort descending
    arrange(desc(Minutes)) %>% 
    rename(
      Date = ts
    ) %>% 
    as.data.frame()
  return(head(days_table, n = top_x))
}
