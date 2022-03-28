# crunch

<!-- badges: start -->
[![R build status](https://github.com/Crunch-io/rcrunch/workflows/R-CMD-check/badge.svg)](https://github.com/Crunch-io/rcrunch/actions)
[![cran](https://www.r-pkg.org/badges/version-last-release/crunch)](https://cran.r-project.org/package=crunch) [![codecov](https://codecov.io/gh/Crunch-io/rcrunch/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/rcrunch)
<!-- badges: end -->

[Cloud Collaboration with Crunch](http://crunch-io.github.io/rcrunch/)

[Crunch.io](https://crunch.io/) provides a cloud-based data store and analytic engine. It has a [web client](https://app.crunch.io/) for interactive data exploration and visualization. The **crunch** package for R allows analysts to interact with and manipulate Crunch datasets from within R. Importantly, this allows technical researchers to collaborate naturally with team members, managers, and clients who prefer a point-and-click interface: because all connect to the same dataset in the cloud, there is no need to email files back and forth continually to share results.

[Subscribe to the mailing list](mailto:rcrunch+subscribe@crunch.io) to receive notification of releases and to ask general support questions.

## Installing

`crunch` can be installed from CRAN with

    install.packages("crunch")

The pre-release version of the package can be pulled from GitHub using the [`remotes`](https://github.com/r-lib/remotes) package (part of `devtools`):

```r
# install.packages("remotes")
remotes::install_github("Crunch-io/rcrunch")
```

## Getting started

The crunch R package needs to know what URL to use for the API and what your token is for authentication.
For more details on how to get an API token, see the [help center article](https://help.crunch.io/hc/en-us/articles/4415963337869-API-Keys).

The `usethis` package can help you set environment environment variables, the following code will
open a text editor:

    $ R
    > if (!require("usethis")) install.packages("usethis")
    > usethis::edit_r_environ()

And then you can add environment variables by adding the following (filling in for `<API-URL>` and `<API-KEY>`).

    R_CRUNCH_API=<API-URL>
    R_CRUNCH_API_KEY=<API-KEY>

Restart your R session, or run command `readRenviron("~/.Renviron")`, and then you will be authenticated automatically.

Check out `listDatasets()` to see the names of your existing datasets, which you can load like `ds <- loadDataset("The name of my dataset")`. New Crunch datasets can be made from a `data.frame`, a .csv or .sav file, or a URL to a file with `newDataset()`. See the help for these functions or [`vignette("crunch")`](https://crunch.io/r/crunch/articles/crunch.html) for more information.

## For developers

`crunch` requires R version 3.0 or greater. Rstudio has instructions for a variety of platforms
both [installing from binaries (recommended)](https://docs.rstudio.com/resources/install-r) or 
[from source](https://docs.rstudio.com/resources/install-r-source/) if necessary.

### Mocks
The tests for the rcrunch package relies on mocks that are compressed into a tarball
so that the package stays within the CRAN 5MB size limit. To avoid git merge conflicts
this file is not checked in so it must be created before you can run tests. It is
created for you if you run `make test`, or you can create it explicitly by running
`make compress-fixtures` or `Rscript dev-misc/compress-fixtures.R`.

### Contributing

See the [contribution guidelines](CONTRIBUTING.md).

### Installing from a local checkout

    # make deps
    # make test

This installs dependencies and then runs the tests, which installs `crunch` from your local checkout in the process. If the dependencies fail to install, check the error message. You may need to install `libcurl` on your system before installing the R packages.

### Running tests

You can run tests in an interactive session, or from the command line, `$ make test` is all you need. Requires the [`httptest`](https://github.com/nealrichardson/httptest) package for R. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=auth`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the [`testthat`](https://testthat.r-lib.org/) package.

Testing has two options: unit tests only, and tests that communicate with an API server. This is governed by an environment variable, `INTEGRATION`, which is false by default, meaning that API integration tests are not run. To modify this and test against the Crunch API, you can run `$ make test INTEGRATION=TRUE`.

To run integration tests, you will need to specify an API key and API server to communicate with. You can do this by setting the environment variables `R_TEST_API` and `CRUNCH_TEST_API_KEY`.

If you are a Crunch developer serving a version of the API/backend with Vagrant or Docker, you will have best results if your R_TEST_API/CRUNCH_TEST_API_KEY (1) is `local.crunch.io`, thanks to a mapping of localhost to that in your hosts file. In order to avoid self-signed certificate errors use the environment variable `R_TEST_VERIFY_SSL=FALSE`. You might point at "https://local.crunch.io:28443/api/", for example. Some tests that cannot run successfully in the local environment will be skipped when run against a local.crunch.io URL.

Example of local usage:

```bash
$ R_TEST_VERIFY_SSL=TRUE CRUNCH_TEST_API_KEY=t0pSecretK3y! R_TEST_API=https://local.crunch.io:28443/api/ make test INTEGRATION=TRUE file=variable-summary
```

### Updating documentation

Run `$ make doc`. Requires the `roxygen2` package.
