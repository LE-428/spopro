{
  #' Function to plot discovery resulting plays by month
  #'
  #' This function extracts the monthly discovery table from `top_discovery_days()`
  #' (or accepts a precomputed monthly table) and plots resulting_plays per month.
  #'
  #' @param df Either the raw dataframe (will call top_discovery_days(df))
  #'           or a dataframe already shaped like top_discovery_days(...)$by_month
  #'           with columns: ts (YYYY-MM or Date) and resulting_plays (numeric).
  #' @return ggplot object (also printed)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
}

discovery_months_plot <- function(df) {
  # If df looks like a raw dataset (has ms_played / master_metadata...),
  # call top_discovery_days and take the by_month element.
  if (is.data.frame(df)) {
    tmp <- top_discovery_times(df)
    if (is.list(tmp) && "by_month" %in% names(tmp)) {
      discovery_months <- tmp$by_month
    }
  } else {
    # Assume df is already the by_month table
    discovery_months <- df
  }
  
  # Ensure ts is character and in YYYY-MM format, then convert to first-of-month Date
  discovery_months <- discovery_months %>%
    mutate(ts = as.character(ts),
           resulting_plays = as.numeric(resulting_plays),
           Timestamp = as.Date(paste0(substr(ts, 1, 7), "-01"), format = "%Y-%m-%d"))
  
  # Create sequence of all months in the range
  all_months <- seq(min(discovery_months$Timestamp, na.rm = TRUE),
                    max(discovery_months$Timestamp, na.rm = TRUE),
                    by = "month")
  all_months_df <- data.frame(Timestamp = all_months)
  
  # Full join to ensure months with zero plays appear
  plot_table <- full_join(all_months_df, discovery_months %>% dplyr::select(Timestamp, resulting_plays),
                          by = "Timestamp") %>%
    mutate(resulting_plays = replace_na(resulting_plays, 0)) %>%
    arrange(Timestamp)
  
  # Plot (same visual style as artist_time_plot)
  p <- ggplot(plot_table, aes(x = Timestamp, y = resulting_plays)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(title = "Discovery resulting plays by month",
         x = "Month",
         y = "Resulting plays") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
  
  print(p)
  invisible(p)
}
