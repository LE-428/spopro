{
#' 
# Filter the entries that were played with incognito mode (private session) enabled
#'
#' @param data_table 
#'
#' @return
#' @export
#'
#' @examples
  

incognito <- function(data_table){
  incognito_table <- subset(data_table, incognito_mode == "TRUE")
  # print(head(incognito_table))
  return(incognito_table)
}

}
