# crunch

<!-- badges: start -->
[![R build status](https://github.com/Crunch-io/rcrunch/workflows/R-CMD-check/badge.svg)](https://github.com/Crunch-io/rcrunch/actions)
[![Build Status](https://travis-ci.org/Crunch-io/rcrunch.png?branch=master)](https://travis-ci.org/Crunch-io/rcrunch) 
[![cran](https://www.r-pkg.org/badges/version-last-release/crunch)](https://cran.r-project.org/package=crunch) [![codecov](https://codecov.io/gh/Crunch-io/rcrunch/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/rcrunch)
<!-- badges: end -->

[Cloud Collaboration with Crunch](http://crunch-io.github.io/rcrunch/)

[Crunch.io](http://crunch.io/) provides a cloud-based data store and analytic engine. It has a [web client](https://app.crunch.io/) for interactive data exploration and visualization. The **crunch** package for R allows analysts to interact with and manipulate Crunch datasets from within R. Importantly, this allows technical researchers to collaborate naturally with team members, managers, and clients who prefer a point-and-click interface: because all connect to the same dataset in the cloud, there is no need to email files back and forth continually to share results.

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

Connecting to Crunch and working with datasets is simple: load the `crunch` package and then `login()`.

    $ R
    > library(crunch)
    > login("jane.r_user@crunch.io")
    Crunch password for jane.r_user@crunch.io:

    You are now logged into Crunch as jane.r_user@crunch.io
    [crunch] >
    ...

If you use OAuth to log into the web application, you'll need to create a Crunch password to use with the R package. To set one, load the package, call `resetPassword("your.email@example.com")`, and check your email for instructions. You may need to log out of your current web app session before you click the link in that email to set your Crunch password.

Once you've logged in, check out `listDatasets()` to see the names of your existing datasets, which you can load like `ds <- loadDataset("The name of my dataset")`. New Crunch datasets can be made from a `data.frame`, a .csv or .sav file, or a URL to a file with `newDataset()`. See the help for these functions or [`vignette("crunch")`](https://crunch.io/r/crunch/articles/crunch.html) for more information.

## Additional configuration

You can set several parameters in your .Rprofile to simplify your workflow:

* `crunch.email` and `crunch.pw`: you can save your Crunch credentials so that you don't have to enter them each time. I.e., you can just `> login()`. Please be advised of the risks of storing your password like this. See `?login` for more information. Also, note that you can opt to store just your `crunch.email` and enter your password each time you log in--a mix of convenience and security.
* `crunch.api`: if not defined, it defaults to "`https://app.crunch.io/api/`", the production server. To specify a different location, either set it in your .Rprofile or after loading the `crunch` package, do `> options(crunch.api="https://otherapi.crunch.io/api/")`.

If you prefer, you can set the environment variables `R_CRUNCH_EMAIL`, `R_CRUNCH_PW`, and `R_CRUNCH_API`.

## For developers

`crunch` requires R version 3.0 or greater. Note that if you're on Ubuntu, particularly older versions, you'll need to add an alternative PPA before trying to `apt-get install` R. For 12.04 Precise, for example,

    # apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
    # echo "deb http://cran.r-project.org/bin/linux/ubuntu precise/" >> /etc/apt/sources.list
    # apt-get update
    # apt-get install libcurl4-openssl-dev
    # sudo apt-get install r-base r-base-dev

See https://cran.r-project.org/bin/linux/ubuntu/README for more details.

### Git hooks

`crunch` uses a git pre-commit hook to ensure that the cube test fixtures in `inst/cubes.tgz` are always up to date. To enable this, you should either copy `hooks/pre-commit` to `.git/hooks/pre-commit` or sym-link the entire hooks directory with `ln -s ../hooks/ .git/hooks`.

### Contributing

See the [contribution guidelines](CONTRIBUTING.md).

### Installing from a local checkout

    # make deps
    # make test

This installs dependencies and then runs the tests, which installs `crunch` from your local checkout in the process. If the dependencies fail to install, check the error message. You may need to install `libcurl` on your system before installing the R packages.

### Running tests

You can run tests in an interactive session, or from the command line, `$ make test` is all you need. Requires the [`httptest`](https://github.com/nealrichardson/httptest) package for R. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=auth`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the [`testthat`](http://testthat.r-lib.org/) package.

Testing has two options: unit tests only, and tests that communicate with an API server. This is governed by an environment variable, `INTEGRATION`, which is false by default, meaning that API integration tests are not run. To modify this and test against the Crunch API, you can run `$ make test INTEGRATION=TRUE`.

To run integration tests, you will need to specify a test user, password, and API server to communicate with, either by setting `test.user`, `test.pw`, and `test.api` in your `.Rprofile`, as in:

```r
options(
    test.user="magic.testuser@crunch.io",
    test.pw="t0pSecretP@ssw0rD",
    test.api="https://app.crunch.io/api/"
)
```

or by setting the environment variables `R_TEST_USER`, `R_TEST_PW`, and `R_TEST_API`.

If you are a Crunch developer serving a version of the API/backend with Vagrant or Docker, you will have best results if your R_TEST_API/test.api (1) is `local.crunch.io`, thanks to a mapping of localhost to that in your hosts file. In order to avoid self-signed certificate errors use the environment variable `R_TEST_VERIFY_SSL=FALSE`. You might point at "https://local.crunch.io:8443/api/", for example. Some tests that cannot run successfully in the local environment will be skipped when run against a local.crunch.io URL.

Example of local usage:

```bash
$ R_TEST_VERIFY_SSL=TRUE R_TEST_USER=magic.testuser@crunch.io R_TEST_PW=t0pSecretP@ssw0rD R_TEST_API=https://local.crunch.io:28443/api/ make test INTEGRATION=TRUE file=variable-summary
```

### Updating documentation

Run `$ make doc`. Requires the `roxygen2` package.
