chunk_to_dir <- function (chunk_name, dir = "R/", extension = ".R") 
{
    for (i in 1:length(chunk_name)) {
        writeLines(paste(chunk_code_get(chunk_name = chunk_name[i]), 
            collapse = "\n"), con = paste0(dir, "/", chunk_name[i], 
            extension))
    }
}


chunk_to_r <- function(chunk_name){
  
  chunk_to_dir(chunk_name = chunk_name)
  
}


chunk_to_tests_testthat <- function (chunk_name) 
{
    chunk_to_dir(chunk_name = chunk_name, dir = "tests/testthat/")
}
