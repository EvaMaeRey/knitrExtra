#' Title
#'
#' @return
#' @export
#'
#' @examples
chunk_names_get <- function(){
  
  rmd_df <- parse_current_rmd()
  
  chunks_info <- subset(rmd_df, !is.na(rmd_df$label)) |> as.data.frame() 
  
  chunks_info[,"label"]
  
}
