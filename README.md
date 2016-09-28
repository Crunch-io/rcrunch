# crunch

[![Build Status](https://travis-ci.org/Crunch-io/rcrunch.png?branch=master)](https://travis-ci.org/Crunch-io/rcrunch)


[Cloud Collaboration with Crunch](http://crunch-io.github.io/rcrunch/)

[Crunch.io](http://crunch.io/) provides a cloud-based data store and analytic engine. It has a [web client](https://beta.crunch.io/) for interactive data exploration and visualization. The **crunch** package for R allows analysts to interact with and manipulate Crunch datasets from within R. Importantly, this allows technical researchers to collaborate naturally with team members, managers, and clients who prefer a point-and-click interface: because all connect to the same dataset in the cloud, there is no need to email files back and forth continually to share results.

[Subscribe to the mailing list](mailto:rcrunch+subscribe@crunch.io) to receive notification of releases and to ask general support questions.

## Installing

`crunch` can be installed from CRAN with

    install.packages("crunch")

The pre-release version of the package can be pulled from GitHub using the [devtools](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    devtools::install_github("Crunch-io/rcrunch", build_vignettes=TRUE)

`crunch` requires R version 3.0 or greater. Note that if you're on Ubuntu, particularly older versions, you'll need to add an alternative PPA before trying to `apt-get install` R. For 12.04 Precise, for example,

    # apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
    # echo "deb http://cran.r-project.org/bin/linux/ubuntu precise/" >> /etc/apt/sources.list
    # apt-get update
    # apt-get install libcurl4-openssl-dev
    # sudo apt-get install r-base r-base-dev

See https://cran.r-project.org/bin/linux/ubuntu/README for more details.

## Getting started

Connecting to Crunch and working with datasets is simple:

    $ R
    > library(crunch)
    > login("jane.r_user@crunch.io")
    Crunch password for jane.r_user@crunch.io:

    You are now logged into Crunch as jane.r_user@crunch.io
    [crunch] >
    ...

Check out `listDatasets()` to see the names of your existing datasets, which you can load like `ds <- loadDataset("The name of my dataset")`. New Crunch datasets can be made from a `data.frame` with `newDataset()`, or from a .csv or .sav file with `newDatasetFromFile()`. See the help for these functions or [`vignette("getting-started", package="crunch")`](inst/doc/getting-started.md) for more information.

## Additional configuration

You can set several parameters in your .Rprofile to simplify your workflow:

* `crunch.email` and `crunch.pw`: you can save your Crunch credentials so that you don't have to enter them each time. I.e., you can just `> login()`. Please be advised of the risks of storing your password like this. See `?login` for more information. Also, note that you can opt to store just your `crunch.email` and enter your password each time you log in--a mix of convenience and security.
* `crunch.api`: if not defined, it defaults to "`https://beta.crunch.io/api/`", the production server. To specify a different location, either set it in your .Rprofile or after loading the `crunch` package, do `> options(crunch.api="https://otherapi.crunch.io/api/")`.

## For developers

### Installing from a local checkout

    # make deps
    # make test

This installs dependencies and then runs the tests, which installs `crunch` from your local checkout in the process. If the dependencies fail to install, check the error message. You may need to install libcurl on your system before installing the R packages.

### Running tests

`$ make test` is all you need. Requires the `testthat` package for R. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=auth`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

Testing has two options: unit tests only, and tests that communicate with an API server. This is governed by an environment variable, `INTEGRATION`. This is false by default, meaning that API integration tests are not run. To modify this and test against the Crunch API, you can run `$ export INTEGRATION=TRUE && make test`.

To run integration tests, you will need to specify a test user, password, and API server to communicate with, either by editing `tests/testthat/helper.R` or by setting `test.user`, `test.pw`, and `test.api` in your `.Rprofile`, as in:

    options(test.user="magic.testuser@crunch.io",
            test.pw="t0pSecretP@ssw0rD",
            test.api="http://local.crunch.io:8080/api/")

or, if you prefer, you can set the environment variables `R_TEST_USER`, `R_TEST_PW`, and `R_TEST_API`.

If INTEGRATION is TRUE, R_TEST_API/test.api hostname **must** be localhost or a hostname that ends with ``crunch.io`` (e.g. ``local.crunch.io``), or login will fail during test-ci. It cannot, for example, be an IP address, but it can be ``localfoo.crunch.io``.  Use your hosts file to assign the IP address to a crunch.io hostname and use that hostname in the R_TEST_API/test.api settings if you must.

If INTEGRATION is TRUE, and your R_TEST_API/test.api URL is an https URL, and you're running the tests against a stack that does not have a valid HTTPS certificate (such as one generated by a crunch ``vagrant provision``), you will receive an error during ``make test-ci`` indicating that the peer certificate is invalid.  Use an http URL instead.

If INTEGRATION is TRUE, and your R_TEST_API/test.api hostname is localhost or local.crunch.io (or any other URL that starts with ``http://local``), some tests that cannot be successfully run on a vagrant host set up via crunch's ``vagrant provision`` will be skipped.

### Updating documentation

Run `$ make doc`. Requires the `roxygen2` package.
