# rcrunch

[Crunch.io](http://crunch.io/) provides a cloud-based data store and analytic engine. It has a [web client](https://beta.crunch.io/) for interactive data exploration and visualization. The **rcrunch** package for R allows analysts to interact with and manipulate Crunch datasets from within R. Importantly, this allows technical researchers to collaborate naturally with team members, managers, and clients who prefer a point-and-click interface: because all connect to the same dataset in the cloud, there is no need to email files back and forth continually to share results. 

[Subscribe to the mailing list](mailto:rcrunch+subscribe@crunch.io) to receive notification of releases and to ask general support questions.

## Installing

You can install the latest package version from within R using the `devtools` package:

    # install.packages("devtools")
    devtools::install_github("Crunch-io/rcrunch")

## Getting started

Connecting to Crunch and working with datasets is simple:

    $ R
    > library(rcrunch)
    > login("jane.r_user@crunch.io")
    Crunch password for jane.r_user@crunch.io: 
    
    You are now logged into Crunch as jane.r_user@crunch.io
    [crunch] > 
    ...

Check out `listDatasets()` to see the names of your existing datasets, which you can load like `ds <- loadDataset("The name of my dataset")`. New Crunch datasets can be made from a `data.frame` with `newDataset()`, or from a .csv or .sav file with `newDatasetFromFile()`. See the help for these functions or `vignette("getting-started", package="rcrunch")` for more information.

## Additional configuration

You can set several parameters in your .Rprofile to simplify your workflow:

* `crunch.email` and `crunch.pw`: you can save your Crunch credentials so that you don't have to enter them each time. I.e., you can just `> login()`. Please be advised of the risks of storing your password like this. See `?login` for more information. Also, note that you can opt to store just your `crunch.email` and enter your password each time you log in--a mix of convenience and security.
* `crunch.api`: if not defined, it defaults to "`https://beta.crunch.io/api/`", the production server. To specify a different location, either set it in your .Rprofile or after loading the `rcrunch` package, do `> options(crunch.api="https://otherapi.crunch.io/api/")`.

## For developers

### Installing from a local checkout

    $ R --slave -e 'install.packages(c("httr", "RJSONIO", "codetools", "testthat"), repo="http://cran.at.r-project.org")'
    $ make test

This installs dependencies and then runs the tests, which installs `rcrunch` in the process.

### Running tests

`$ make test` is all you need. Requires the `testthat` package for R. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=auth`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

Testing has two options: unit tests only, and tests that communicate with an API server. This is governed by an environment variable, `LOCALONLY`. This is true by default, meaning that API integration tests are not run. To modify this and test against the Crunch API, you can run `$ export LOCALONLY=FALSE && make test`. 

To run integration tests, you will need to specify a test user, password, and API server to communicate with, either by editing `pkg/inst/tests/helper.R` or by setting `test.user`, `test.pw`, and `test.api` in your `.Rprofile`, as in:

    options(test.user="magic.testuser@crunch.io",
            test.pw="t0pSecretP@ssw0rD",
            test.api="http://local.crunch.io:8080/api/")


### Updating documentation

Run `$ make doc`. Requires the `roxygen2` package.
