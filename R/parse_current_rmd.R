parse_current_rmd <- function(){
  
    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents
    
    tmp <- tempfile()
    writeLines(source, tmp)
    
    lightparser::split_to_tbl(tmp)

}    
