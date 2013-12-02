# rcrunch

R package for interacting with the Crunch API

## Installing

`$ R CMD INSTALL pkg` installs from source. Or you could run the tests, which installs the package in the process...

## Running tests

`$ make test` is all you need. Does require the `testthat` package for R. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=auth`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

Testing has two options: unit tests only, and tests that communicate with an API server. To enable the latter, set the logical parameter `run.local.tests.only` to `FALSE` in `pkg/inst/tests/helper.R`. You will also need to specify a test user, password, and API server to communicate with, either by editing `helper.R` further or by setting `test.user`, `test.pw`, and `test.api` in your `.Rprofile`.

## Updating documentation

Run `$ make doc`. Requires the `devtools` and `roxygen2` packages.

## Getting started

### Configuration

You can set several parameters in your .Rprofile to simplify your workflow:

* `crunch.api.endpoint`: if not defined, it defaults to "`http://localhost:8080/api/`", the local development location. To specify a different location, either set it in your .Rprofile or after loading the `rcrunch` package, do `> options(crunch.api.endpoint="https://otherapi.crunch.io/api/")`.
* `crunch.email` and `crunch.pw`: you can save your Crunch credentials so that you don't have to enter them each time. I.e., you can just `> login()`. Please be advised of the risks of storing your password like this. See `?login` for more information. Also, note that you can opt to store just your `crunch.email` and enter your password each time you log in--a mix of convenience and security.

### Firing up R

Connecting to Crunch and working with datasets is simple:

    $ R
    > library(rcrunch)
    > login("jane.r_user@crunch.io")
    Crunch password for jane.r_user@crunch.io: 
    
    You are now logged into Crunch as jane.r_user@crunch.io
    [crunch] > 
    ...

Check out `listDatasets()` to see the names of your existing datasets, which you can load like `ds <- loadDataset("The name of my dataset")`. New Crunch datasets can be made from `data.frames` with `newDataset()`, or from a .csv or .sav file with `newDatasetFromFile()`. See the help for these functions for more information.