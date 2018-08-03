# Setup is executed when tests are run, but not when loadall is run

# find a file that is either in the package root or inst folders while testing
find_file <- function(file_name) {
    pth <- system.file(file_name, package = "crunch")
    if (nchar(pth)) {
        return(pth)
    }
    ## Else, look for it. hack for devtools::test / testthat::test_package
    pths <- file.path(testthat::test_path("..", ".."), c("", "inst"), file_name)
    return(pths[file.exists(pths)])
}

# Our "test package" common harness code
source(find_file("crunch-test.R"), local = TRUE)
