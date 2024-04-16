
  - [Part 0. Proposal](#part-0-proposal)
  - [Part I. Work out functionality 🚧
    ✅](#part-i-work-out-functionality--)
      - [Try it out](#try-it-out)
  - [Part II. Packaging and documentation 🚧
    ✅](#part-ii-packaging-and-documentation--)
      - [Phase 1. Minimal working
        package](#phase-1-minimal-working-package)
      - [Phase 3: Settling and testing 🚧
        ✅](#phase-3-settling-and-testing--)
      - [Phase 4. Promote to wider audience… 🚧
        ✅](#phase-4-promote-to-wider-audience--)
      - [Phase 5: Harden/commit: Submit to CRAN/RUniverse 🚧
        ✅](#phase-5-hardencommit-submit-to-cranruniverse--)
  - [Appendix: Reports, Environment](#appendix-reports-environment)
      - [Description file complete? 🚧 ✅](#description-file-complete--)
      - [Environment 🚧 ✅](#environment--)
      - [`devtools::check()` report](#devtoolscheck-report)
      - [Package directory file tree](#package-directory-file-tree)

# Part 0. Proposal

Proposing the {knitrExtra} package\! 🦄
<!-- (typical package introduction write up; but actually aspirational) -->

The goal of {knitrExtra} is to make some of my favorite functionality a
little more accessible and usable interactively (in RStudio, I’m pretty
much piggy backing on Kelly Bodwin’s work on this).

Without the package, we live in the effort-ful world that follows 🏋:

``` r
knitr::knit_code$get("times_two") |> as.vector()
#> NULL
knitr::knit_code$get() |> names()
#>  [1] "unnamed-chunk-1"           "unnamed-chunk-2"          
#>  [3] "chunk_code_get_static"     "chunk_names_get_static"   
#>  [5] "unnamed-chunk-3"           "unnamed-chunk-4"          
#>  [7] "unnamed-chunk-5"           "unnamed-chunk-6"          
#>  [9] "unnamed-chunk-7"           "chunk_names_get"          
#> [11] "chunk_to_dir"              "unnamed-chunk-8"          
#> [13] "unnamed-chunk-9"           "unnamed-chunk-10"         
#> [15] "test_calc_times_two_works" "unnamed-chunk-11"         
#> [17] "unnamed-chunk-12"          "unnamed-chunk-13"         
#> [19] "unnamed-chunk-14"          "unnamed-chunk-15"         
#> [21] "unnamed-chunk-16"
```

With the {xxxx} package, we’ll live in a different world (🦄 🦄 🦄) where
the task is a snap 🫰:

Proposed API:

``` 

library(xxxxx)

xxxxx::times_two(x = 4)
```

# Part I. Work out functionality 🚧 ✅

Here is a function that will do some work…

``` r
chunk_code_get_static <- function(chunk_name){
  
  knitr::knit_code$get(chunk_name) |> as.vector()
  
}
```

``` r
chunk_names_get_static <- function(){
  
  knitr::all_labels()
  
}
```

```` r
knitr::knit_code$get()
#> $`unnamed-chunk-1`
#> [1] "knitr::opts_chunk$set(" "  collapse = TRUE,"     "  comment = \"#>\","   
#> [4] "  eval = T"             ")"                     
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$include
#> [1] FALSE
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-1"
#> 
#> 
#> $`unnamed-chunk-2`
#> [1] "knitr::knit_code$get(\"times_two\") |> as.vector()"
#> [2] "knitr::knit_code$get() |> names()"                 
#> [3] ""                                                  
#> [4] ""                                                  
#> [5] ""                                                  
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-2"
#> 
#> 
#> $chunk_code_get_static
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"                                                
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "chunk_code_get_static"
#> 
#> 
#> $chunk_names_get_static
#> [1] "chunk_names_get_static <- function(){"
#> [2] "  "                                   
#> [3] "  knitr::all_labels()"                
#> [4] "  "                                   
#> [5] "}"                                    
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "chunk_names_get_static"
#> 
#> 
#> $`unnamed-chunk-3`
#> [1] "knitr::knit_code$get()"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-3"
#> 
#> 
#> $`unnamed-chunk-4`
#> [1] "chunk_code_get_static(\"chunk_code_get_static\")" 
#> [2] "chunk_code_get_static(\"chunk_names_get_static\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-4"
#> 
#> 
#> $`unnamed-chunk-5`
#>   [1] "# Awesome!"                                                                               
#>   [2] "check_is_live <- function(){"                                                             
#>   [3] "  "                                                                                       
#>   [4] "  is_live <- FALSE"                                                                       
#>   [5] "  "                                                                                       
#>   [6] "  # Check to see if we're in editor context"                                              
#>   [7] "  if (requireNamespace(\"rstudioapi\", quietly = TRUE) &&"                                
#>   [8] "      rstudioapi::isAvailable()) {"                                                       
#>   [9] ""                                                                                         
#>  [10] "    is_live <- tryCatch({"                                                                
#>  [11] "      rstudioapi::getSourceEditorContext()"                                               
#>  [12] "      TRUE"                                                                               
#>  [13] "    }, error = function(e) FALSE)"                                                        
#>  [14] ""                                                                                         
#>  [15] "  }  "                                                                                    
#>  [16] "  "                                                                                       
#>  [17] "  return(is_live)"                                                                        
#>  [18] "  "                                                                                       
#>  [19] "}"                                                                                        
#>  [20] ""                                                                                         
#>  [21] "# so cool!"                                                                               
#>  [22] "text_chunk_extract <- function(.text, chunk_name) {"                                      
#>  [23] ""                                                                                         
#>  [24] "  # Find the start of the desired chunk"                                                  
#>  [25] "  chunk_regex <- paste0('\\\\`\\\\`\\\\`\\\\{[A-z]+ ', chunk_name, '(\\\\}|(,.*\\\\}))$')"
#>  [26] ""                                                                                         
#>  [27] "  start_chunk <- .text |>"                                                                
#>  [28] "    stringr::str_which(chunk_regex)"                                                      
#>  [29] ""                                                                                         
#>  [30] "  if (length(start_chunk) == 0) {"                                                        
#>  [31] ""                                                                                         
#>  [32] "    stop(paste0(\"Error: No chunk found with name '\", chunk_name, \"'\"))"               
#>  [33] ""                                                                                         
#>  [34] "  } else if (length(start_chunk) > 1) {"                                                  
#>  [35] ""                                                                                         
#>  [36] "    stop(paste0(\"Error: Duplicate chunk name '\", chunk_name, \"'\"))"                   
#>  [37] ""                                                                                         
#>  [38] "  }"                                                                                      
#>  [39] ""                                                                                         
#>  [40] "  end_chunk <- .text[-c(1:start_chunk)] |>"                                               
#>  [41] "    stringr::str_which(stringr::fixed(\"```\")) |>"                                       
#>  [42] "    min() + start_chunk"                                                                  
#>  [43] ""                                                                                         
#>  [44] "  chunk_text <- .text[(start_chunk):(end_chunk)] |>"                                      
#>  [45] "    stringr::str_c(collapse = \"\\n\")"                                                   
#>  [46] ""                                                                                         
#>  [47] "  attributes(chunk_text) <- NULL"                                                         
#>  [48] ""                                                                                         
#>  [49] "  return(chunk_text)"                                                                     
#>  [50] ""                                                                                         
#>  [51] "}"                                                                                        
#>  [52] ""                                                                                         
#>  [53] "chunk_remove_fencing_and_options <- function(code_chunk){"                                
#>  [54] "  "                                                                                       
#>  [55] "  # does not yet, in fact, remove options like these: "                                   
#>  [56] "  # | my-chunk, echo = FALSE, fig.width = 10,"                                            
#>  [57] "  # | fig.cap = \"This is a long long"                                                    
#>  [58] "  # |   long long caption.\""                                                             
#>  [59] "  "                                                                                       
#>  [60] " chunk_as_vec <- stringr::str_split(code_chunk,\"\\\\n\")[[1]] "                          
#>  [61] " "                                                                                        
#>  [62] " # remove fencing which are first and last lines"                                         
#>  [63] " return(chunk_as_vec[2:(length(chunk_as_vec)-1)])"                                        
#>  [64] "  "                                                                                       
#>  [65] "}"                                                                                        
#>  [66] ""                                                                                         
#>  [67] "# wow!"                                                                                   
#>  [68] "return_chunk_code_live <- function(chunk_name) {"                                         
#>  [69] ""                                                                                         
#>  [70] "  "                                                                                       
#>  [71] "    ed        <- rstudioapi::getSourceEditorContext()"                                    
#>  [72] "    source    <- ed$contents"                                                             
#>  [73] ""                                                                                         
#>  [74] "    # can we use knitr tools to directly parse source for us? "                           
#>  [75] "    # tmp       <- tempfile()"                                                            
#>  [76] "    # writeLines(source, tmp)"                                                            
#>  [77] "    # readLines(tmp)"                                                                     
#>  [78] "    # knitr::knit_code$get(name = tmp)"                                                   
#>  [79] "    "                                                                                     
#>  [80] "    my_code_chunk  <- text_chunk_extract(.text = source, chunk_name)"                     
#>  [81] ""                                                                                         
#>  [82] "    # If neither of those worked, error"                                                  
#>  [83] "    if (is.null(my_code_chunk)) {"                                                        
#>  [84] ""                                                                                         
#>  [85] "    stop(paste0(\"Error: No chunk found with name '\", chunk_name, \"'\"))"               
#>  [86] ""                                                                                         
#>  [87] "    }"                                                                                    
#>  [88] ""                                                                                         
#>  [89] "    # remove chunk fencing, first and last lines"                                         
#>  [90] "    my_code <- chunk_remove_fencing_and_options(my_code_chunk)"                           
#>  [91] "    "                                                                                     
#>  [92] "    return(my_code)"                                                                      
#>  [93] "  "                                                                                       
#>  [94] "}"                                                                                        
#>  [95] ""                                                                                         
#>  [96] "#' Title"                                                                                 
#>  [97] "#'"                                                                                       
#>  [98] "#' @param chunk_name a character string with the name of the chunk of interest"           
#>  [99] "#'"                                                                                       
#> [100] "#' @return a vector of the code contained in the referenced chunk"                        
#> [101] "#' @export "                                                                              
#> [102] "#'"                                                                                       
#> [103] "#' @examples"                                                                             
#> [104] "chunk_code_get <- function(chunk_name){"                                                  
#> [105] "  "                                                                                       
#> [106] "  is_live <- check_is_live()"                                                             
#> [107] "  "                                                                                       
#> [108] "  if(is_live){"                                                                           
#> [109] "    return_chunk_code_live(chunk_name)"                                                   
#> [110] "  }else{"                                                                                 
#> [111] "  chunk_code_get_static(chunk_name = chunk_name)"                                         
#> [112] "    }"                                                                                    
#> [113] ""                                                                                         
#> [114] "}"                                                                                        
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-5"
#> 
#> 
#> $`unnamed-chunk-6`
#> [1] "chunk_code_get(\"chunk_code_get_static\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-6"
#> 
#> 
#> $`unnamed-chunk-7`
#>  [1] "chunk_names_get_live <- function(chunk_name) {"                     
#>  [2] ""                                                                   
#>  [3] "    ed        <- rstudioapi::getSourceEditorContext()"              
#>  [4] "    source    <- ed$contents"                                       
#>  [5] ""                                                                   
#>  [6] "    "                                                               
#>  [7] "    first_fence <- source[grep(\"\\\\`\\\\`\\\\`\\\\{r \", source)]"
#>  [8] "    "                                                               
#>  [9] "    names_of_named_chunks <- first_fence |> "                       
#> [10] "      stringr::str_remove(\"\\\\`\\\\`\\\\`\\\\{r \") |>"           
#> [11] "      stringr::str_remove(\",.+\")"                                 
#> [12] ""                                                                   
#> [13] "    names_of_named_chunks"                                          
#> [14] "    "                                                               
#> [15] "}"                                                                  
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-7"
#> 
#> 
#> $chunk_names_get
#>  [1] "chunk_names_get <- function(){" "  "                            
#>  [3] "  is_live <- check_is_live()"   "  "                            
#>  [5] "  if(is_live){"                 "    chunk_names_get_live()"    
#>  [7] "  }else{"                       "  chunk_names_get_static()"    
#>  [9] "    }"                          ""                              
#> [11] "}"                             
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "chunk_names_get"
#> 
#> 
#> $chunk_to_dir
#> [1] "chunk_to_dir <- function (chunk_name, dir = \"R/\", extension = \".R\") " 
#> [2] "{"                                                                        
#> [3] "    for (i in 1:length(chunk_name)) {"                                    
#> [4] "        writeLines(paste(chunk_code_get(chunk_name = chunk_name[i]), "    
#> [5] "            collapse = \"\\n\"), con = paste0(dir, \"/\", chunk_name[i], "
#> [6] "            extension))"                                                  
#> [7] "    }"                                                                    
#> [8] "}"                                                                        
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "chunk_to_dir"
#> 
#> 
#> $`unnamed-chunk-8`
#>  [1] "devtools::create(\".\") # Bit 1. 1X"                                                                  
#>  [2] "### Bit 2a: dependencies to functions using '::' syntax to pkg functions "                            
#>  [3] "usethis::use_package(\"ggplot2\") # Bit 2b: document dependencies"                                    
#>  [4] "readme2pkg::chunk_to_r(chunk_name = \"times_two\") # Bit 3: send code chunk with function to R folder"
#>  [5] "devtools::check(pkg = \".\")  # Bit 4: check that package is minimally viable"                        
#>  [6] "devtools::install(pkg = \".\", upgrade = \"never\") # Bit 5: install package locally"                 
#>  [7] "usethis::use_lifecycle_badge(\"experimental\") # Bit 6: add lifecycle badge"                          
#>  [8] "# Bit 7 (below): Write traditional readme"                                                            
#>  [9] "# Bit 8: Compile readme"                                                                              
#> [10] "# Bit 9: Push to githup"                                                                              
#> [11] "# Bit 10: listen and iterate"                                                                         
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-8"
#> 
#> 
#> $`unnamed-chunk-9`
#> [1] "library(mypackage)  ##<< change to your package name here"
#> [2] "mypackage:::times_two(10)"                                
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-9"
#> 
#> 
#> $`unnamed-chunk-10`
#> [1] "usethis::use_mit_license()"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-10"
#> 
#> 
#> $test_calc_times_two_works
#> [1] "library(testthat)"                   ""                                   
#> [3] "test_that(\"calc times 2 works\", {" "  expect_equal(times_two(4), 8)"    
#> [5] "  expect_equal(times_two(5), 10)"    "  "                                 
#> [7] "})"                                 
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "test_calc_times_two_works"
#> 
#> attr(,"chunk_opts")$eval
#> F
#> 
#> 
#> $`unnamed-chunk-11`
#> [1] "readme2pkg::chunk_to_tests_testthat(\"test_calc_times_two_works\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-11"
#> 
#> 
#> $`unnamed-chunk-12`
#> [1] "devtools::check(pkg = \".\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-12"
#> 
#> 
#> $`unnamed-chunk-13`
#> [1] "readLines(\"DESCRIPTION\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-13"
#> 
#> 
#> $`unnamed-chunk-14`
#> [1] "all <- sessionInfo() |> print() |> capture.output()"
#> [2] "all[11:17]"                                         
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-14"
#> 
#> 
#> $`unnamed-chunk-15`
#> [1] "devtools::check(pkg = \".\")"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$eval
#> F
#> 
#> attr(,"chunk_opts")$error
#> T
#> 
#> attr(,"chunk_opts")$results
#> [1] "hide"
#> 
#> attr(,"chunk_opts")$warning
#> F
#> 
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-15"
#> 
#> 
#> $`unnamed-chunk-16`
#> [1] "fs::dir_tree(recurse = T)"
#> attr(,"chunk_opts")
#> attr(,"chunk_opts")$label
#> [1] "unnamed-chunk-16"
````

## Try it out

``` r
chunk_code_get_static("chunk_code_get_static")
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"
chunk_code_get_static("chunk_names_get_static")
#> [1] "chunk_names_get_static <- function(){"
#> [2] "  "                                   
#> [3] "  knitr::all_labels()"                
#> [4] "  "                                   
#> [5] "}"
```

```` r
# Awesome!
check_is_live <- function(){
  
  is_live <- FALSE
  
  # Check to see if we're in editor context
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {

    is_live <- tryCatch({
      rstudioapi::getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)

  }  
  
  return(is_live)
  
}

# so cool!
text_chunk_extract <- function(.text, chunk_name) {

  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .text |>
    stringr::str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .text[-c(1:start_chunk)] |>
    stringr::str_which(stringr::fixed("```")) |>
    min() + start_chunk

  chunk_text <- .text[(start_chunk):(end_chunk)] |>
    stringr::str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

chunk_remove_fencing_and_options <- function(code_chunk){
  
  # does not yet, in fact, remove options like these: 
  # | my-chunk, echo = FALSE, fig.width = 10,
  # | fig.cap = "This is a long long
  # |   long long caption."
  
 chunk_as_vec <- stringr::str_split(code_chunk,"\\n")[[1]] 
 
 # remove fencing which are first and last lines
 return(chunk_as_vec[2:(length(chunk_as_vec)-1)])
  
}

# wow!
return_chunk_code_live <- function(chunk_name) {

  
    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents

    # can we use knitr tools to directly parse source for us? 
    # tmp       <- tempfile()
    # writeLines(source, tmp)
    # readLines(tmp)
    # knitr::knit_code$get(name = tmp)
    
    my_code_chunk  <- text_chunk_extract(.text = source, chunk_name)

    # If neither of those worked, error
    if (is.null(my_code_chunk)) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

    }

    # remove chunk fencing, first and last lines
    my_code <- chunk_remove_fencing_and_options(my_code_chunk)
    
    return(my_code)
  
}

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
    return_chunk_code_live(chunk_name)
  }else{
  chunk_code_get_static(chunk_name = chunk_name)
    }

}
````

``` r
chunk_code_get("chunk_code_get_static")
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"
```

``` r
chunk_names_get_live <- function(chunk_name) {

    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents

    
    first_fence <- source[grep("\\`\\`\\`\\{r ", source)]
    
    names_of_named_chunks <- first_fence |> 
      stringr::str_remove("\\`\\`\\`\\{r ") |>
      stringr::str_remove(",.+")

    names_of_named_chunks
    
}
```

``` r
chunk_names_get <- function(){
  
  is_live <- check_is_live()
  
  if(is_live){
    chunk_names_get_live()
  }else{
  chunk_names_get_static()
    }

}
```

``` r
chunk_to_dir <- function (chunk_name, dir = "R/", extension = ".R") 
{
    for (i in 1:length(chunk_name)) {
        writeLines(paste(chunk_code_get(chunk_name = chunk_name[i]), 
            collapse = "\n"), con = paste0(dir, "/", chunk_name[i], 
            extension))
    }
}
```

# Part II. Packaging and documentation 🚧 ✅

## Phase 1. Minimal working package

### Bit A. Created package archetecture, running `devtools::create(".")` in interactive session. 🚧 ✅

``` r
devtools::create(".") # Bit 1. 1X
### Bit 2a: dependencies to functions using '::' syntax to pkg functions 
usethis::use_package("ggplot2") # Bit 2b: document dependencies
readme2pkg::chunk_to_r(chunk_name = "times_two") # Bit 3: send code chunk with function to R folder
devtools::check(pkg = ".")  # Bit 4: check that package is minimally viable
devtools::install(pkg = ".", upgrade = "never") # Bit 5: install package locally
usethis::use_lifecycle_badge("experimental") # Bit 6: add lifecycle badge
# Bit 7 (below): Write traditional readme
# Bit 8: Compile readme
# Bit 9: Push to githup
# Bit 10: listen and iterate
```

### Bit 7. Write traditional README that uses built package (also serves as a test of build). 🚧 ✅

The goal of the {xxxx} package is to …

Install package with:

    remotes::install_github("GithubCoolUser/mypacakge")

Once functions are exported you can remove go to two colons, and when
things are are really finalized, then go without colons (and rearrange
your readme…)

``` r
library(mypackage)  ##<< change to your package name here
mypackage:::times_two(10)
```

## Phase 3: Settling and testing 🚧 ✅

### Bit A. Added a description and author information in the [DESCRIPTION file](https://r-pkgs.org/description.html) 🚧 ✅

### Bit B. Added [roxygen skeleton](https://r-pkgs.org/man.html)? 🚧 ✅

### Bit C. Chosen a [license](https://r-pkgs.org/license.html)? 🚧 ✅

``` r
usethis::use_mit_license()
```

### Bit D. Settle on [examples](https://r-pkgs.org/man.html#sec-man-examples). Put them in the roxygen skeleton and readme. 🚧 ✅

### Bit E. Written formal [tests](https://r-pkgs.org/testing-basics.html) of functions and save to test that folders 🚧 ✅

That would look like this…

``` r
library(testthat)

test_that("calc times 2 works", {
  expect_equal(times_two(4), 8)
  expect_equal(times_two(5), 10)
  
})
```

``` r
readme2pkg::chunk_to_tests_testthat("test_calc_times_two_works")
```

### Bit F. Check again. Addressed notes, warnings and errors. 🚧 ✅

``` r
devtools::check(pkg = ".")
```

## Phase 4. Promote to wider audience… 🚧 ✅

### Bit A. Package website built? 🚧 ✅

### Bit B. Package website deployed? 🚧 ✅

## Phase 5: Harden/commit: Submit to CRAN/RUniverse 🚧 ✅

# Appendix: Reports, Environment

## Description file complete? 🚧 ✅

``` r
readLines("DESCRIPTION")
```

## Environment 🚧 ✅

Here I just want to print the packages and the versions

``` r
all <- sessionInfo() |> print() |> capture.output()
all[11:17]
#> [1] ""                                                                         
#> [2] "attached base packages:"                                                  
#> [3] "[1] stats     graphics  grDevices utils     datasets  methods   base     "
#> [4] ""                                                                         
#> [5] "loaded via a namespace (and not attached):"                               
#> [6] " [1] compiler_4.2.2  fastmap_1.1.1   cli_3.6.1       tools_4.2.2    "     
#> [7] " [5] htmltools_0.5.4 rstudioapi_0.14 yaml_2.3.7      rmarkdown_2.20 "
```

## `devtools::check()` report

``` r
devtools::check(pkg = ".")
```

## Package directory file tree

``` r
fs::dir_tree(recurse = T)
#> .
#> ├── README.Rmd
#> ├── README.md
#> └── readme2pkg.template.Rproj
```
