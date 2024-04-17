chunk_names_get_live <- function(chunk_name) {

    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents

    
    first_fence <- source[grep("\\`\\`\\`\\{r ", source)]
    
    names_of_named_chunks <- first_fence |> 
      stringr::str_remove("\\`\\`\\`\\{r ") |>
      stringr::str_remove(",.+") |>
      stringr::str_remove("\\}")

    names_of_named_chunks
    
}
