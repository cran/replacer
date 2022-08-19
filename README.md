
<!-- README.md is generated from README.Rmd. Please edit that file -->

# replacer

<!-- badges: start -->
<!-- badges: end -->

<font font_family="courier new">`replacer`</font>, a value replacement
utility currently based on package
[<font family="courier">`data.table`</font>](https://CRAN.R-project.org/package=data.table),
is intended for outside-database update of data. It requires the
preparation of a *lookup* file which is a list of replacement *requests*
with and, in special circumstances, without an *index* column.

This utility is accessible to beginners to `R` and facilitates complex
dataset updates with minimal prompt time by employing User-friendly
functions which automatically follow a decision tree rooted in User’s
input.

Data processing such as data cleanup, file format conversion and the
appendage of new data to data files are outside the scope.

## Installation

You can install the released version of replacer from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("replacer")
```

## Example

Below is a basic example which shows the screen result of data
replacement using a lookup file without index:

``` r
require(replacer)
#> Loading required package: replacer
## basic example code
dir = system.file('extdata', package = 'replacer')
## update the 'data' dataset with new replacement values
replaceVals(dir, save = FALSE) 
#> 
#> reading data from: C:/R/R-4.1.2/library/replacer/extdata...
#> data reading complete ...
#> checking standard columns in lookup ...
#> starting replacements ...
#> found duplicates and simple replacements but no index in lookup :
#> 
#>  would recommend User-made index
#> proceeding any way ...
#> subsetting lookup and creating index ...
#> found request for 1:1 replacements: proceeding ...
#> completed 1:1 replacements ...
#> rejoining columns uninvolved in simple replacements ...
#> searching for 1:many replacements ...
#> found request for multiple duplicated value replacments: creating index ...
#> replacing multiple duplicated values ...
#> found request for multiple missing value replacements: replacing ...
#> processed 1:many replacements ...
#> there are still missing values in some involved columns!
#> 
#> helper function has completed!
#> $` updated_data_using_lookup`
#>                a       b       c    d
#>  1:           aa   7.174   0.259   11
#>  2: NOT  PRESENT  17.572   0.478 5555
#>  3:  DUP VALUE_1   8.888   0.707   NA
#>  4:           dd   0.794   0.737 5555
#>  5:           UU -13.964 999.000 5555
#>  6:  DUP VALUE_1   0.127   0.737    8
#>  7:           ff   8.836   0.476 5555
#>  8: NOT  PRESENT   6.397  -0.001    8
#>  9:           hh  -4.979   0.121    9
#> 10: NOT  PRESENT  -2.755  -0.227    8
#> 11:           EE   1.111  -0.127    6
#> 12: NOT  PRESENT   1.121   0.333    7
#> 13: NOT  PRESENT  -4.979   0.737 5555
#> 14: NOT  PRESENT 999.000   0.476 5555
#> 15: NOT  PRESENT   1.111 999.000    8
#> 16:           JJ   1.121 999.000 5555
#> 
#> $` multiple_dups_repl_counts`
#>    vars oldVals     newVals  a  b  d
#> 1:    a      cc DUP VALUE_1  2 NA NA
#> 2:    b   2.142       1.111 NA  2 NA
#> 3:    d      15        5555 NA NA  7
#> 
#> $` NAs_remaining`
#> a b c d 
#> 0 0 0 1
```

Familiarity with basic `R` commands is necessary. Familiarity with
`data.table` is not, yet strongly recommended!

Users are encouraged to start by reading the
[vignette](https://mran.microsoft.com/web/packages/replacer/vignettes/readmefirst.html).
