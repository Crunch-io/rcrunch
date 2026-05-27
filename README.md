# crunch.lake

<!-- badges: start -->
<!-- badges: end -->

The goal of crunch.lake is to explore how the crunch lake APIs should be wrapped 
for R.

## Installation

You can install the development version of crunch.lake from [GitHub](https://github.com/) with:

``` r
# This will work if you have your github PAT set up correctly in R (I follow instructions in `usethis::create_github_token()`)
remotes::install_github("Crunch-io/r-packages/packages/crunch.lake")
```

## Example
This is how you download the metadata, schema, and data files for a dataset and read them into R:

``` r
library(crunch.lake)

# See https://crunch.io/api/reference/#post-/datasources/download/ for information about `export_filter`
# and other API options.
# Be careful, all parameters are JSONified and the API is the typical level of python strict, so
# you want to ensure vectors remain vectors, and you don't send `null` instead of leaving a key off.
files <- cr_export(ds, export_filter = list(variables = I(c("wave", "age"))))

md <- files |> cr_read_meta()
data <- files |> cr_read_data()
```

