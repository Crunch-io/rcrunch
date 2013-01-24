context("making a new dataset")

test_that("Source file can be uploaded if logged in", {
    login("***REMOVED***")
    testfile <- system.file("fake.csv", package="rcrunch", mustWork=TRUE)
    expect_true(createSource(testfile, response.handler=function (response) response$status_code==201))
})

test_that("Source file cannot be uploaded if not logged in", {
    logout()
    testfile <- system.file("fake.csv", package="rcrunch", mustWork=TRUE)
    expect_error(createSource(testfile), 
        "You must authenticate before making this request")
})

