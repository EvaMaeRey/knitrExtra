chunk_names_get <- function(){
  
  is_live <- check_is_live()
  
  if(is_live){
    chunk_names_get_live()
  }else{
  chunk_names_get_static()
    }

}
