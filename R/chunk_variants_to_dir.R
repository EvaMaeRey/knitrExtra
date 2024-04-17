chunk_variants_to_dir <- function (chunk_name, chunk_name_suffix = "_variants", 
                                   file_name = NULL, 
    dir = "R/", replace1, replacements1, replace2 = NULL, replacements2 = NULL, 
    replace3 = NULL, replacements3 = NULL, replace4 = NULL, replacements4 = NULL) {
    template <- return_chunk_code(chunk_name)
    script_contents <- c()
    if (is.null(file_name)) {
        file_name <- paste0(chunk_name, chunk_name_suffix, ".R")
    }
    for (i in 1:length(replacements1)) {
        template_mod <- stringr::str_replace_all(template, replace1, 
            replacements1[i])
        if (!is.null(replace2)) {
            template_mod <- stringr::str_replace_all(template_mod, 
                replace2, replacements2[i])
        }
        if (!is.null(replace3)) {
            template_mod <- stringr::str_replace_all(template_mod, 
                replace3, replacements3[i])
        }
        if (!is.null(replace4)) {
            template_mod <- stringr::str_replace_all(template_mod, 
                replace4, replacements4[i])
        }
        script_contents <- c(script_contents, template_mod)
    }
    writeLines(script_contents, paste0(dir, file_name))
}
