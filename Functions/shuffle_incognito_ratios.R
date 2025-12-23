# Prints the ratio of shuffled streams and streams with incognito mode enabled

# shuffle_incognito_ratios(df)
# total_streams shuffle_ratio incognito_ratio
# 1         58557     0.2076097      0.01475485

shuffle_incognito_ratios <- function(df) {
  df <- drop_podcasts(df)
  
  out_table <- df %>%
    dplyr::select(incognito_mode, shuffle) %>%
    dplyr::summarise(
      total_streams   = dplyr::n(),
      shuffle_ratio   = mean(shuffle == TRUE, na.rm = TRUE),
      incognito_ratio = mean(incognito_mode == TRUE, na.rm = TRUE)
    )
  
  return(out_table)
}
