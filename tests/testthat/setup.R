# Setup is executed when tests are run, but not when loadall is run

# Our "test package" common harness code
crunch_test_path <- system.file("crunch-test.R", package="crunch")
if (crunch_test_path == "") {
    # hack for devtools::test / testthat::test_package
    crunch_test_path <- find_file("crunch-test.R")
}

source(crunch_test_path, local = TRUE)

