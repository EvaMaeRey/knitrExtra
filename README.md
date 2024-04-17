
  - [Part 0. Proposal](#part-0-proposal)
  - [Part I. Work out functionality 🚧
    ✅](#part-i-work-out-functionality--)
      - [Try it out](#try-it-out)
  - [Try it out\! Should return equivelantly in live session or
    ‘knitted’
    version.](#try-it-out-should-return-equivelantly-in-live-session-or-knitted-version)
  - [Return chunk names](#return-chunk-names)
  - [Code from chunks to files](#code-from-chunks-to-files)
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
much piggy backing on Kelly Bodwin’s vision and work on this - led the
way as to how-to w/ rstudio API).

Without the package, we live in the effort-ful world that follows 🏋:

``` r
# grabbing code from a chunk: 
knitr::knit_code$get("chunk_code_get_static") |> as.vector()
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"

# getting the names of chunks:
knitr::knit_code$get() |> names()
#>  [1] "unnamed-chunk-1"           "unnamed-chunk-2"          
#>  [3] "chunk_code_get_static"     "unnamed-chunk-3"          
#>  [5] "liveness_helpers"          "chunk_code_get_live"      
#>  [7] "chunk_code_get"            "unnamed-chunk-4"          
#>  [9] "chunk_names_get_static"    "chunk_names_get_live"     
#> [11] "chunk_names_get"           "chunk_to_dir"             
#> [13] "chunk_variants_to_dir"     "unnamed-chunk-5"          
#> [15] "unnamed-chunk-6"           "unnamed-chunk-7"          
#> [17] "test_calc_times_two_works" "unnamed-chunk-8"          
#> [19] "unnamed-chunk-9"           "unnamed-chunk-10"         
#> [21] "unnamed-chunk-11"          "unnamed-chunk-12"         
#> [23] "unnamed-chunk-13"

# sending code from a chunk to a stand alone file
knitr::knit_code$get("chunk_code_get_static") |> 
  as.vector() |> 
  writeLines("R/chunk_code_get_static.R")
```

And *importantly*, we can’t access chunk names from within a live .Rmd,
or the code from chunks in the document we are working on. But this kind
of interactivity can be useful.

With the {knitrExtra} package, we’ll live in a different world (🦄 🦄 🦄)
where the task is a snap 🫰 and interactivity is provided (from within
RStudio IDE - Borrowing from Kelly Bodwin’s approach in flair):

Proposed API:

``` 

library(knitrExtra)

knitrExtra::chunk_code_get("chunk_code_get_static")

knitrExtra::chunk_names_get()

knitrExtra::chunk_to_r("chunk_code_get_static")
```

# Part I. Work out functionality 🚧 ✅

First, let’s just create a designated function for getting code from a
chunk via the knit\_code$get method.

``` r
chunk_code_get_static <- function(chunk_name){
  
  knitr::knit_code$get(chunk_name) |> as.vector()
  
}
```

## Try it out

If we knit our document we’ll see that these functions work

``` r
chunk_code_get_static("chunk_code_get_static")
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"
```

Now, both of the above functions work in a static context. To have get
the same behavior ‘live’, we’ll use roughly Kelly Bodwin’s approach
which takes advantage of the rstudioapi package.

First here are some helper functions…

``` r
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
    stringr::str_which(stringr::fixed("^\\`\\`\\`")) |>
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
```

Then we write the live analogue to chunk\_code\_get\_static

``` r
# wow!
chunk_code_get_live <- function(chunk_name) {

  
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
```

And we finally combine the static and live versions into one function…
`chunk_code_get()` which is exported.

``` r
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
```

# Try it out\! Should return equivelantly in live session or ‘knitted’ version.

``` r
chunk_code_get("chunk_code_get_static")
#> [1] "chunk_code_get_static <- function(chunk_name){"   
#> [2] "  "                                               
#> [3] "  knitr::knit_code$get(chunk_name) |> as.vector()"
#> [4] "  "                                               
#> [5] "}"
```

# Return chunk names

First we just alias knitr::all\_label() to a function that’s named more
in line with others in this package.

``` r
chunk_names_get_static <- function(){
  
  knitr::all_labels()
  
}
```

Then we ust the rstudioapi package to look at the live document and pull
out chunk names (just using regular expressions - don’t love this… Seems
like source should be saved as a temp file and evaluated directly by
knitr tools?)

``` r
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
```

We combine the static and live versions into `chunk_names_get()` which
should be exported. Note that the live v. static behavior is currently
different and only named chunks will appear in the live list. This isn’t
ideal, but it’s where the project is right now.

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

# Code from chunks to files

It is nice to be able to grab code from chunks and send them to files
for the purpose of building packages from a single file like a readme.
`chunk_to_dir` exists for this purpose. The defaults are that you are
sending code from a package readme to an .R file in the R package
folder.

``` r
#' Title
#'
#' @param chunk_name 
#' @param dir 
#' @param extension 
#'
#' @return
#' @export
#'
#' @examples
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
```

Finally, functionality (and the implementation) that I’m uncertain about
is `chunk_variants_to_dir()` This is an interesting meta programming
solution, perhaps.

``` r
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
```

# Part II. Packaging and documentation 🚧 ✅

## Phase 1. Minimal working package

### Bit A. Created package archetecture, running `devtools::create(".")` in interactive session. 🚧 ✅

``` r
devtools::create(".") # Bit 1. 1X
### Bit 2a: dependencies to functions using '::' syntax to pkg functions 
usethis::use_package("knitr") # Bit 2b: document dependencies
usethis::use_package("stringr") # Bit 2b: document dependencies
usethis::use_package("rstudioapi") # Bit 2b: document dependencies
chunk_names_get()
readme2pkg::chunk_to_r(
  chunk_name = c("chunk_code_get_static" , "liveness_helpers",   
                 "chunk_code_get_live", "chunk_code_get", 
                 "chunk_names_get_static", "chunk_names_get_live", 
                 "chunk_names_get", "chunk_to_dir",
                 "chunk_variants_to_dir")) 
# Bit 3: send code chunk with function to R folder
devtools::check(pkg = ".")  # Bit 4: check that package is minimally viable; document's as a pre-step
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
library(knitrExtra)  ##<< change to your package name here
knitrExtra:::check_is_live()
knitrExtra:::chunk_code_get()

getNamespaceExports("knitrExtra")
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
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> ├── R
#> │   ├── chunk_code_get.R
#> │   ├── chunk_code_get_live.R
#> │   ├── chunk_code_get_static.R
#> │   ├── chunk_names_get.R
#> │   ├── chunk_names_get_live.R
#> │   ├── chunk_names_get_static.R
#> │   ├── chunk_to_dir.R
#> │   ├── chunk_variants_to_dir.R
#> │   └── liveness_helpers.R
#> ├── README.Rmd
#> ├── README.md
#> ├── knitrExtra.Rproj
#> ├── man
#> │   ├── chunk_code_get.Rd
#> │   └── chunk_to_dir.Rd
#> └── readme2pkg.template.Rproj
```
