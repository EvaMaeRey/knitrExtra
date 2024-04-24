#' Title
#'
#' @param chunk_name a character string with the name of the chunk of interest
#'
#' @return a vector of the code contained in the referenced chunk
#' @export 
#'
#' @examples
chunk_code_get <- function(chunk_name = "chunk_code_get"){
  
  rmd_df <- parse_current_rmd()
  
  chunk_info <- subset(rmd_df, rmd_df$label == chunk_name) 
  
  chunk_info[,"code"][[1]][[1]] |> as.vector()
  
}
