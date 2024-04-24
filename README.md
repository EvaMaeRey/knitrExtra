
  - [Part 0. Proposal](#part-0-proposal)
  - [Part I. Work out functionality 🚧
    ✅](#part-i-work-out-functionality--)
      - [Try it out](#try-it-out)
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

``` 

# grabbing code from a chunk: 
knitr::knit_code$get("chunk_code_get_static") |> as.vector()

# getting the names of chunks:
knitr::knit_code$get() |> names()

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

First, a helper function from the lightparser

``` r
parse_current_rmd <- function(){
  
    ed        <- rstudioapi::getSourceEditorContext()
    source    <- ed$contents
    
    tmp <- tempfile()
    writeLines(source, tmp)
    
    lightparser::split_to_tbl(tmp)

}    
```

``` r
parse_current_rmd()  
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> # A tibble: 98 × 8
#>    type    label          params       text  code  heading heading_level section
#>    <chr>   <chr>          <list>       <nam> <lis> <chr>           <dbl> <chr>  
#>  1 yaml    <NA>           <named list> <lgl> <lgl>  <NA>              NA  <NA>  
#>  2 inline  <NA>           <lgl [1]>    <chr> <lgl>  <NA>              NA  <NA>  
#>  3 block   unnamed-chunk… <named list> <lgl> <chr>  <NA>              NA  <NA>  
#>  4 inline  <NA>           <lgl [1]>    <chr> <lgl>  <NA>              NA  <NA>  
#>  5 heading <NA>           <lgl [1]>    <chr> <lgl> "Part …             1 "Part …
#>  6 inline  <NA>           <lgl [1]>    <chr> <lgl>  <NA>              NA "Part …
#>  7 heading <NA>           <lgl [1]>    <chr> <lgl> "grabb…             1 "grabb…
#>  8 inline  <NA>           <lgl [1]>    <chr> <lgl>  <NA>              NA "grabb…
#>  9 heading <NA>           <lgl [1]>    <chr> <lgl> "getti…             1 "getti…
#> 10 inline  <NA>           <lgl [1]>    <chr> <lgl>  <NA>              NA "getti…
#> # ℹ 88 more rows
```

``` r
#' Title
#'
#' @param chunk_name a character string with the name of the chunk of interest
#'
#' @return a vector of the code contained in the referenced chunk
#' @export 
#'
#' @examples
chunk_code_get <- function(chunk_name = "chunk_code_get"){
  
  rmd_df <- parse_current_rmd()
  
  chunk_info <- subset(rmd_df, rmd_df$label == chunk_name) 
  
  chunk_info[,"code"][[1]][[1]] |> as.vector()
  
}
```

## Try it out

If we knit our document we’ll see that these functions work

``` r
chunk_code_get("chunk_code_get")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#>  [1] "#' Title"                                                                      
#>  [2] "#'"                                                                            
#>  [3] "#' @param chunk_name a character string with the name of the chunk of interest"
#>  [4] "#'"                                                                            
#>  [5] "#' @return a vector of the code contained in the referenced chunk"             
#>  [6] "#' @export "                                                                   
#>  [7] "#'"                                                                            
#>  [8] "#' @examples"                                                                  
#>  [9] "chunk_code_get <- function(chunk_name = \"chunk_code_get\"){"                  
#> [10] "  "                                                                            
#> [11] "  rmd_df <- parse_current_rmd()"                                               
#> [12] "  "                                                                            
#> [13] "  chunk_info <- subset(rmd_df, rmd_df$label == chunk_name) "                   
#> [14] "  "                                                                            
#> [15] "  chunk_info[,\"code\"][[1]][[1]] |> as.vector()"                              
#> [16] "  "                                                                            
#> [17] "}"
```

# Return chunk names

First we just alias knitr::all\_label() to a function that’s named more
in line with others in this package.

``` r
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
```

``` r
chunk_names_get()
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#>  [1] "unnamed-chunk-1"           "parse_current_rmd"        
#>  [3] "unnamed-chunk-2"           "chunk_code_get"           
#>  [5] "unnamed-chunk-3"           "chunk_names_get"          
#>  [7] "unnamed-chunk-4"           "chunk_to_dir"             
#>  [9] "unnamed-chunk-5"           "chunk_variants_to_dir"    
#> [11] "unnamed-chunk-6"           "unnamed-chunk-7"          
#> [13] "unnamed-chunk-8"           "unnamed-chunk-9"          
#> [15] "unnamed-chunk-10"          "test_calc_times_two_works"
#> [17] "unnamed-chunk-11"          "unnamed-chunk-12"         
#> [19] "unnamed-chunk-13"          "unnamed-chunk-14"         
#> [21] "unnamed-chunk-15"          "unnamed-chunk-16"
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
        writeLines(
          paste(chunk_code_get(chunk_name = chunk_name[i]), 
            collapse = "\n"), 
          con = paste0(dir, "/", chunk_name[i], extension))
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

``` r
chunk_to_dir("chunk_to_dir")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

Finally, functionality (and the implementation) that I’m uncertain about
is `chunk_variants_to_dir()` This is an interesting meta programming
solution, perhaps.

``` r
chunk_variants_to_dir <- function (chunk_name, chunk_name_suffix = "_variants", 
                                   file_name = NULL, 
    dir = "R/", replace1, replacements1, replace2 = NULL, replacements2 = NULL, 
    replace3 = NULL, replacements3 = NULL, replace4 = NULL, replacements4 = NULL) {
    template <- chunk_code_get(chunk_name)
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
# devtools::create(".") # Bit 1. 1X
### Bit 2a: dependencies to functions using '::' syntax to pkg functions 
usethis::use_package("rstudioapi") # Bit 2b: document dependencies
usethis::use_package("stringr") # Bit 2b: document dependencies
usethis::use_dev_package(package = "lightparser", remote = "ThinkR-open/lightparser")

chunk_names_get()
```

``` r
chunk_to_r("parse_current_rmd")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
chunk_to_r("chunk_code_get")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
chunk_to_r("chunk_names_get")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
chunk_to_r("chunk_to_dir")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
chunk_to_r("chunk_variants_to_dir") 
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

``` r
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
knitrExtra::chunk_names_get()
knitrExtra::chunk_code_get("chunk_to_dir")
knitrExtra:::parse_current_rmd()


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
#> [6] " [1] lightparser_0.0.1 ps_1.7.2          fansi_1.0.5       utf8_1.2.3       "
#> [7] " [5] digest_0.6.31     R6_2.5.1          lifecycle_1.0.3   magrittr_2.0.3   "
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
#> │   ├── chunk_names_get.R
#> │   ├── chunk_to_dir.R
#> │   ├── chunk_variants_to_dir.R
#> │   └── parse_current_rmd.R
#> ├── README.Rmd
#> ├── README.md
#> ├── knitrExtra.Rproj
#> ├── man
#> │   ├── chunk_code_get.Rd
#> │   ├── chunk_names_get.Rd
#> │   └── chunk_to_dir.Rd
#> └── readme2pkg.template.Rproj
```
