{
#' 
# Filter the entries that were played with incognito mode (private session) enabled
#'
#' @param data_frame 
#'
#' @return
#' @export
#'
#' @examples
  

incognito <- function(data_frame){
  incognito_table <- subset(data_frame, incognito_mode == "TRUE")
  # print(head(incognito_table))
  return(incognito_table)
}

}
