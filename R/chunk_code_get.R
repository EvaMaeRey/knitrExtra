#' Title
#'
#' @param chunk_name a character string with the name of the chunk of interest
#'
#' @return a vector of the code contained in the referenced chunk
#' @export 
#'
#' @examples
chunk_code_get <- function(chunk_name){
  
  is_live <- check_is_live()
  
  if(is_live){
  chunk_code_get_live(chunk_name)
  }else{
  chunk_code_get_static(chunk_name = chunk_name)
    }

}
