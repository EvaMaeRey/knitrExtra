chunk_code_get_static <- function(chunk_name){
  
  knitr::knit_code$get(chunk_name) |> as.vector()
  
}
